module PGSimple
    (
    -- * Fields
      InetText(..)
    -- * Application classes
    , HasPostgres(..)
    -- * Wrapped functions
    , pgQuery, pgQuery_
    , pgReturning
    , pgExecute, pgExecute_, pgExecuteMany
    -- * Monadic db
    , PgMonad(..)
    , launchPG
    -- * Entity abstraction
    , Entity(..)
    , Ent
    , ToMarkedRow(..)
    , selectEntity
    , insertEntity
    , pgSelectEntities
    , pgSelectJustEntities
    , pgInsertEntity
    , pgInsertManyEntities
    , pgGetEntity
    , pgGetEntityBy
    , pgDeleteEntity
    , pgUpdateEntity
    , pgSelectCount
    , mSelectEntities
    , mSelectJustEntities
    , mInsertEntity
    , mInsertManyEntities
    , mGetEntity
    , mGetEntityBy
    , mDeleteEntity
    , mUpdateEntity
    , mSelectCount
    -- * auxiliary Query functions
    , dquo
    , qIntercalate
    , concFields
    -- * Some re-exports
    , Connection, connect, defaultConnectInfo, connectPostgreSQL
    , ToField(..), ToRow(..), FromField(..), FromRow(..)
    , Query(..), Only(..), (:.)(..)
    ) where

import Prelude

import Control.Applicative ( (<$>) )
import Control.Monad ( unless )
import Control.Monad.Catch
    (MonadCatch(..), MonadMask(..), onException)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (ReaderT(..), ask)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson.Types
import Data.Int (Int64)
import Data.Maybe ( listToMaybe )
import Data.Monoid ( Monoid(..), (<>) )
import Data.Pool (withResource, Pool)
import Data.Proxy
import Data.String ( IsString )
import Data.Typeable ( Typeable, typeRep )
import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.FromField
    ( FromField(..), returnError,  ResultError(..), typename )
import Database.PostgreSQL.Simple.FromRow ( FromRow(..) )
import Database.PostgreSQL.Simple.ToField ( ToField(..), Action )
import Database.PostgreSQL.Simple.ToRow ( ToRow(..) )
import Database.PostgreSQL.Simple.Transaction
    (TransactionMode, beginMode, defaultTransactionMode)
import Database.PostgreSQL.Simple.Types ( Query(..) )
import Text.Blaze ( ToMarkup )

import qualified Data.ByteString as BS
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | type to put and get from db 'inet' and 'cidr' fields
newtype InetText =
    InetText
    { unInetText :: T.Text
    } deriving ( IsString, Eq, Ord, Read, Show, Typeable
               , Monoid, ToField, ToJSON, FromJSON, ToMarkup )

instance FromField InetText where
    fromField fld Nothing = returnError ConversionFailed
                            fld "can not convert Null to InetText"
    fromField fld (Just bs) = do
        n <- typename fld
        case n of
            "inet" -> handle
            "cidr" -> handle
            _ -> returnError
                 ConversionFailed fld
                 "could not convert to InetText"
      where
        handle = return $ InetText
                 $ T.decodeUtf8 bs


class MonadIO m => HasPostgres m where
    getPGpool :: m (Pool Connection)


-- | Typeclass for monad which can execute postgres queries
class (Monad m) => PgMonad m where
    mQuery               :: (ToRow q, FromRow r) => Query ->  q  -> m [r]
    mQuery_              :: (FromRow r)          => Query       -> m [r]
    mReturning           :: (ToRow q, FromRow r) => Query -> [q] -> m [r]
    mExecute             :: (ToRow q)            => Query ->  q  -> m Int64
    mExecute_            ::                        Query       -> m Int64
    mExecuteMany         :: (ToRow q)            => Query -> [q] -> m Int64

    -- | Run several queries in transaction. Commit transaction if everything
    -- ok. Rollback if any exception catched.
    -- Usage example:
    --
    -- @
    -- handler :: Handler Id
    -- handler = launchPG $ mWithTransaction defaultTransactionMode $ do
    --     [aid] <- mExecute "INSERT INTO tbl(val) VALUES (?) RETURNING id" [10]
    --     mExecuteMany "INSERT INTO tbl2(tbl_id, val) VALUES (?, ?)"
    --         $ zipWith (,) (repeat aid) [1..10]
    --     return aid
    -- @
    --
    -- After executing we will have either exception thrown either new 11 rows
    -- in database.
    mWithTransactionMode :: TransactionMode -> m a -> m a

    -- | The same as 'mWithTransactionMode' but use some default transaction
    -- mode
    mWithTransaction     :: m a -> m a
    -- | Start transaction in some mode
    mBeginMode           :: TransactionMode -> m ()
    mBegin               :: m ()
    mCommit              :: m ()
    mRollback            :: m ()
    -- FIXME: implement mWithSavepoint       :: m a -> m a

instance (MonadIO m, MonadCatch m, MonadMask m, Connection ~ con)
         => PgMonad (ReaderT con m) where
    mQuery q ps = do
        con <- ask
        liftIO $ query con q ps
    mQuery_ q = do
        con <- ask
        liftIO $ query_ con q
    mReturning q ps = do
        con <- ask
        liftIO $ returning con q ps
    mExecute q ps = do
        con <- ask
        liftIO $ execute con q ps
    mExecute_ q = do
        con <- ask
        liftIO $ execute_ con q
    mExecuteMany q ps = do
        con <- ask
        liftIO $ executeMany con q ps

    mWithTransactionMode mode act = do --  NOTE: copy-pasted
          mask $ \restore -> do
              mBeginMode mode
              r <- restore act `onException` mRollback
              mCommit
              return r
    mWithTransaction act = mWithTransactionMode defaultTransactionMode act
    mBeginMode mode = ask >>= liftIO . beginMode mode
    mBegin = ask >>= liftIO . begin
    mCommit = ask >>= liftIO . commit
    mRollback = ask >>= liftIO . rollback


-- | Use 'HasPostgres' instnace to run 'ReaderT Connection m' monad.
-- Usage example:
--
-- @
-- handler :: Handler [Int]
-- handler = launchPG $ do
--     mExecute "INSERT INTO tbl(val) values (?)" [10]
--     a <- mQuery_ "SELECT val FROM tbl"
--     return a
-- @
launchPG :: (HasPostgres m, MonadBaseControl IO m)
         => ReaderT Connection m a
         -> m a
launchPG act = do
    pool <- getPGpool
    withResource pool $ \ con -> runReaderT act con

-- | Internal helper for querying stuff. Do not leak!
withPG :: HasPostgres m => (Connection -> IO a) -> m a
withPG action = do
    pool <- getPGpool
    -- TODO: catch query errors instead of just lifting IO.
    liftIO $ withResource pool action

pgQuery :: (HasPostgres m, ToRow q, FromRow r)
        => Query -> q -> m [r]
pgQuery q ps = withPG $ \c -> query c q ps

pgQuery_ :: (HasPostgres m, FromRow r)
         => Query -> m [r]
pgQuery_ q = withPG $ \c -> query_ c q

pgReturning :: (HasPostgres m, ToRow q, FromRow r)
            => Query -> [q] -> m [r]
pgReturning q ps = withPG $ \c -> returning c q ps

pgExecute :: (HasPostgres m, ToRow q)
          => Query -> q -> m Int64
pgExecute q ps = withPG $ \c -> execute c q ps

pgExecute_ :: (HasPostgres m)
           => Query -> m Int64
pgExecute_ q = withPG $ \c -> execute_ c q

pgExecuteMany :: (HasPostgres m, ToRow q)
              => Query -> [q] -> m Int64
pgExecuteMany q ps = withPG $ \c -> executeMany c q ps

-- | enclose field/table identifier with double quotes. It does not check if
-- query string is already quoted
dquo :: Query -> Query
dquo a = "\"" <> a <> "\""

-- | Intercalate query string like 'BS.intercalate' does
qIntercalate :: Query -> [Query] -> Query
qIntercalate (Query q) qs =
    Query
    $ BS.intercalate q
    $ map fromQuery qs

-- | Auxiliary typeclass for data types which can map to rows of some
-- table. This typeclass is used inside functions like 'pgSelectEntities' to
-- generate queries.
class Entity a where
    -- | Id type for this entity
    type EntityId a :: *
    -- | Table name of this entity
    tableName :: Proxy a -> Query
    -- | Field names without 'id' and 'created'. The order of field names must match
    -- with order of fields in 'ToRow' and 'FromRow' instances of this type.
    fieldNames :: Proxy a -> [Query]

type Ent a = (EntityId a, a)


-- | Auxiliary typeclass used inside such functions like
-- 'pgUpdateEntity'. Instance of this typeclass must be convertable to arbitrary
-- list of pairs (field name, field value).
--
-- @
-- data UpdateAppForm =
--     UpdateAppForm
--     { uafActive    :: !(Maybe Bool)
--     , uafPublished :: !(Maybe Bool)
--     } deriving (Eq, Ord, Typeable)
--
-- instance ToMarkedRow UpdateAppForm where
--     toMarkedRow f =
--         catMaybes
--         [ ((const "active") &&& toField) <$> uafActive f
--         , ((const "published") &&& toField) <$> uafPublished f
--         ]
-- @
--
-- So, no we can update our app like that:
--
-- @
-- pgUpdateEntity aid
--     (Proxy :: Proxy ClientApp)
--     (UpdateAppForm Nothing (Just True))
-- @
--
-- This is especially usable, when 'UpdateAppForm' is constructed from HTTP
-- query.
class ToMarkedRow a where
    -- | generate list of pairs (field name, field value)
    toMarkedRow :: a -> [(Query, Action)]

instance ToMarkedRow [(Query, Action)] where
    toMarkedRow = id

-- | Generate comma separated double-quoted field names
--
-- @
-- λ> concFields Nothing ["fld", "fld2"]
-- "\"fld\", \"fld2\""
-- λ> concFields (Just "table") ["fld", "fld2"]
-- "\"table\".\"fld\", \"table\".\"fld2\""
-- @
concFields :: (Maybe Query)   -- ^ prefix for each field (i.e. when you use join)
           -> [Query]         -- ^ to get field names from
           -> Query
concFields pref qs = qIntercalate ", "
                     $ map (maybe id qPrepend pref)
                     $ map dquo qs
  where
    qPrepend p val = (dquo p) <> "." <> val

-- | Auxiliary function. Generate select query
-- @
-- λ> selectFields False Nothing ["field", "field2"] "table"
-- "SELECT \"field\", \"field2\" FROM table"
-- λ> selectFields True Nothing ["field", "field2"] "table"
-- "SELECT DISTINCT \"field\", \"field2\" FROM table"
-- λ> selectFields False (Just "table") ["field", "field2"] "table"
-- "SELECT \"table\".\"field\", \"table\".\"field2\" FROM table"
-- λ> selectFields True (Just "table") ["field", "field2"] "table"
-- "SELECT DISTINCT \"table\".\"field\", \"table\".\"field2\" FROM table"
-- @
selectFields :: Bool             -- ^ distinct ?
             -> (Maybe Query)    -- ^ namespace for field names
             -> [Query]          -- ^ fields
             -> Query            -- ^ table name
             -> Query
selectFields distinct mpre flds tbl =
    mconcat
    [ "SELECT "
    , if distinct then "DISTINCT " else ""
    , concFields mpre flds
    , " FROM "
    , tbl ]

-- | Generate SELECT query string
--
-- @
-- data Tbl = Tbl Int Int
--
-- instance Entity Tbl where
--     type EntityId Tbl = Int
--     tableName _ = "tbl"
--     fieldNames _ = ["fld1", "fld2"]
--
-- λ> selectEntity False Nothing id (Proxy :: Proxy Tbl)
-- "SELECT \"fld1\", \"fld2\" FROM tbl"
-- λ> selectEntity False Nothing ("id":) (Proxy :: Proxy Tbl)
-- "SELECT \"id\", \"fld1\", \"fld2\" FROM tbl"
-- λ> selectEntity False (Just "t") ("id":) (Proxy :: Proxy Tbl)
-- "SELECT \"t\".\"id\", \"t\".\"fld1\", \"t\".\"fld2\" FROM tbl"
-- @
selectEntity :: (Entity a)
             => Bool                -- ^ distinct?
             -> (Maybe Query)       -- ^ namespace for fields
             -> ([Query] -> [Query]) -- ^ append/prepend some fields to the
                                   -- query. (i.e. "id")
             -> Proxy a
             -> Query
selectEntity distinct mpre qfun a =
    selectFields distinct mpre
    (qfun $ fieldNames a)
    $ tableName a

-- | Same as 'selectEntity' but generates INSERT query
insertEntity :: (Entity a) => Proxy a -> Query
insertEntity a =
    mconcat
    [ "INSERT INTO "
    , dquo $ tableName a
    , "("
    , concFields Nothing $ fieldNames a
    , ") values ("
    , qIntercalate ", "
      $ map (const "?")
      $ fieldNames a
    , ")" ]

pgInsertEntity :: forall a m. (HasPostgres m, Entity a,
                         ToRow a, FromField (EntityId a))
               => a
               -> m (EntityId a)
pgInsertEntity a = someInsertEntity pgQuery a

mInsertEntity :: forall a m. (Entity a, PgMonad m, ToRow a, FromField (EntityId a))
              => a
              -> m (EntityId a)
mInsertEntity a = someInsertEntity mQuery a


-- | Auxiliary function to abstract off the query generation. Used to create
-- functions 'pgInsertEntity' and 'mInsertEntity'. First argument is a query
-- executor, usually 'pgQuery' or 'mQuery'
someInsertEntity :: forall a m. (Monad m, Entity a, ToRow a, FromField (EntityId a))
                 => (Query -> a -> m ([Only (EntityId a)])) -- query executor
                 -> a                                     -- entity
                 -> m (EntityId a)
someInsertEntity actor a = do
    [Only ret] <- actor insertEnt a
    return ret
  where
    insertEnt =
        mconcat
        [ insertEntity (Proxy :: Proxy a)
        , " RETURNING id" ]

-- | Select entities as pairs of (id, entity).
--
-- @
-- handler :: Handler [Ent a]
-- handler = do
--     pgSelectEntities False Nothing
--         "WHERE field = ? ORDER BY field2" [10]
--
-- handler2 :: Handler [Ent a]
-- handler2 = do
--     pgSelectEntities False (Just "t")
--         (mconcat
--          [ " AS t INNER JOIN table2 AS t2 "
--          , " ON t.t2_id = t2.id "
--           " WHERE t.field = ? ORDER BY t2.field2" ])
--         [10]
--    -- Here the query will be: SELECT ... FROM tbl AS t INNER JOIN ...
-- @
pgSelectEntities :: forall m a q. (Functor m, HasPostgres m, Entity a,
                             FromRow a, ToRow q, FromField (EntityId a))
                 => Bool           -- ^ distinct?
                 -> (Maybe Query) -- ^ namespace for each field
                 -> Query         -- ^ WHERE clause or whatever after SELECT .. FROM
                 -> q             -- ^ parameters for query
                 -> m [Ent a]
pgSelectEntities distinct mpre q prms =
    someSelectEntities pgQuery distinct mpre q prms


-- | Same as 'pgSelectEntities' but must be run inside PgMonad, like
--
-- @
-- handler :: Handler [Ent a]
-- handler = launchPG $ do
--     mSelectEntities False Nothing
--         "WHERE field = ? ORDER BY field2" [10]
-- @
mSelectEntities :: forall m a q. (Entity a, PgMonad m, ToRow q, FromRow a,
                            FromField (EntityId a), Functor m)
                => Bool
                -> Maybe Query
                -> Query
                -> q
                -> m [Ent a]
mSelectEntities distinct mpre q prms =
    someSelectEntities mQuery distinct mpre q prms

someSelectEntities :: forall m a q. (Functor m, Entity a, FromRow a, ToRow q, FromField (EntityId a))
                 => (Query -> q -> m [(Only (EntityId a)) :. a])
                 -> Bool           -- ^ distinct?
                 -> (Maybe Query) -- ^ namespace for each field
                 -> Query         -- ^ WHERE clause or whatever after SELECT .. FROM
                 -> q             -- ^ parameters for query
                 -> m [Ent a]
someSelectEntities actor distinct mpre q prms = do
    map toTuples
        <$> actor selectQ prms
  where
    toTuples ((Only eid) :. entity) = (eid, entity)
    selectQ =
        mconcat
        [ selectEntity distinct mpre ("id":)
          (Proxy :: Proxy a)
        , " "
        , q ]

-- | Same as 'pgSelectEntities' but do not select id
pgSelectJustEntities :: forall m a q. (Functor m, HasPostgres m, Entity a, FromRow a, ToRow q)
                     => Bool     -- ^ distinct ?
                     -> (Maybe Query) -- ^ namespace for fields
                     -> Query        -- ^ WHERE clause or whatever after SELECT .. FROM
                     -> q            -- ^ parameters for query
                     -> m [a]
pgSelectJustEntities distinct mpre q prms =
    someSelectJustEntities pgQuery distinct mpre q prms

-- | Same as 'mSelectEntities' but do not select id
mSelectJustEntities :: forall m a q. (Entity a, PgMonad m, ToRow q, FromRow a, Functor m)
                    => Bool
                    -> Maybe Query
                    -> Query
                    -> q
                    -> m [a]
mSelectJustEntities distinct mpre q prms =
    someSelectJustEntities mQuery distinct mpre q prms

someSelectJustEntities :: forall m a q. (Functor m, Monad m, Entity a, FromRow a, ToRow q)
                       => (Query -> q -> m [a]) -- ^ query executor
                       -> Bool     -- ^ distinct ?
                       -> (Maybe Query) -- ^ namespace for fields
                       -> Query        -- ^ WHERE clause or whatever after SELECT .. FROM
                       -> q            -- ^ parameters for query
                       -> m [a]
someSelectJustEntities actor distinct mpre q prms = do
    actor selectQ prms
  where
    selectQ =
        mconcat
        [ selectEntity distinct mpre id
          (Proxy :: Proxy a)
        , " "
        , q ]

-- | Select entity by id
--
-- @
-- getUser :: EntityId User ->  Handler User
-- getUser uid = do
--     pgGetEntity uid
--         >>= maybe notFound return
-- @
pgGetEntity :: forall m a. (ToField (EntityId a), Entity a,
                      HasPostgres m, FromRow a, Functor m)
            => EntityId a
            -> m (Maybe a)
pgGetEntity eid = someGetEntity pgQuery eid

-- | Same for PgMonad
--
-- @
-- getUser :: EntityId User ->  Handler User
-- getUser uid = do
--     r <- launchPG $ mWithTransaction $ do
--         mInsertEntity $ Action
--             $ "tried to get user with uid"
--             <> show uid
--         mGetEntity uid
--     maybe notFound return r
-- @
mGetEntity :: forall m a. (ToField (EntityId a), Entity a,
                     PgMonad m, FromRow a, Functor m)
           => EntityId a
           -> m (Maybe a)
mGetEntity eid = someGetEntity mQuery eid

someGetEntity :: forall m a. (Functor m, Entity a, FromRow a, ToField (EntityId a))
              => (Query -> Only (EntityId a) -> m [a])
              -> EntityId a
              -> m (Maybe a)
someGetEntity actor eid =
    listToMaybe <$> actor selectQ (Only eid)
  where
    selectQ =
        mconcat
        [ selectEntity False Nothing id
          (Proxy :: Proxy a)
        , "  WHERE id = ? LIMIT 1 " ]

-- | Get entity by some fields constraint
--
-- @
-- getUser :: UserName -> Handler User
-- getUser name = do
--     pgGetEntityBy
--         [("name" :: Query, toField name),
--          ("active", toField True)]
--         >>= maybe notFound return
-- @
--
-- The query here will be like
--
-- @
-- pgQuery "SELECT id, name, phone ... FROM users WHERE name = ? AND active = ?" (name, True)
-- @
pgGetEntityBy :: forall m a b. (Entity a, HasPostgres m, ToMarkedRow b,
                          FromField (EntityId a), FromRow a, Functor m)
              => b               -- ^ uniq constrained list of fields and values
              -> m (Maybe (Ent a))
pgGetEntityBy f = someGetEntityBy pgQuery f

mGetEntityBy :: forall m a b. (Entity a, PgMonad m, ToMarkedRow b,
                         FromField (EntityId a), FromRow a, Functor m)
             => b                -- ^ uniq constrained list of fields and values
             -> m (Maybe (Ent a))
mGetEntityBy f = someGetEntityBy mQuery f

someGetEntityBy :: forall m a b. (Functor m, Entity a, FromRow a,
                            FromField (EntityId a), ToMarkedRow b)
                => (Query -> [Action] -> m [(Only (EntityId a)) :. a])
                -> b             -- ^ constrain row
                -> m (Maybe (Ent a))
someGetEntityBy actor row =
    ((fmap toTuple) . listToMaybe)
    <$> actor selectQ (map snd fields)
  where
    fields = toMarkedRow row
    toTuple ((Only eid) :. e) = (eid, e)
    selectQ =
        mconcat
        [ selectEntity False Nothing ("id":)
          (Proxy :: Proxy a)
        , " WHERE "
        , conditions
        , " LIMIT 1" ]
    conditions = qIntercalate " AND "
                 $ map ((\f -> (dquo f) <> " = ?") . fst)
                 fields

someInsertManyEntities :: forall a m. (Monad m, Entity a, ToRow a)
                       => (Query -> [a] -> m Int64) -- ^ query executor
                       -> [a]                     -- ^ entity
                       -> m ()
someInsertManyEntities actor a = do
    _ <- actor (insertEntity (Proxy :: Proxy a)) a
    return ()

-- | Same as 'pgInsertEntity' but insert many entities at on action
pgInsertManyEntities :: forall a m. (Entity a, HasPostgres m, ToRow a)
                     => [a]
                     -> m ()
pgInsertManyEntities a = someInsertManyEntities pgExecuteMany a


mInsertManyEntities :: forall a m. (Entity a, PgMonad m, ToRow a)
                    => [a]
                    -> m ()
mInsertManyEntities a = someInsertManyEntities mExecuteMany a


someDeleteEntity :: forall a m. (Entity a, ToField (EntityId a), Functor m)
                 => (Query -> Only (EntityId a) -> m Int64)
                 -> EntityId a
                 -> (Proxy a) --  FIXME: to remove proxy EntityId must be
                                     --  data family, not type family
                 -> m ()
someDeleteEntity actor eid a = fmap (const ())
                               $ actor q
                               $ Only eid
  where
    q = mconcat
        [ "DELETE FROM "
        , dquo $ tableName a
        , " WHERE id = ? " ]

-- | Delete entity. Requires 'Proxy' while 'EntityId' is type family, but should
-- be a data family
--
-- @
-- rmUser :: EntityId User -> Handler ()
-- rmUser uid = do
--     pgDeleteEntity uid
--         (Proxy :: Proxy User)
-- @
pgDeleteEntity :: forall a m. (Entity a, HasPostgres m, ToField (EntityId a), Functor m)
               => EntityId a
               -> Proxy a
               -> m ()
pgDeleteEntity eid a = someDeleteEntity pgExecute eid a

mDeleteEntity :: forall a m. (Entity a, PgMonad m, ToField (EntityId a), Functor m)
              => EntityId a
              -> Proxy a
              -> m ()
mDeleteEntity eid a = someDeleteEntity mExecute eid a

someUpdateEntity :: forall a b m. (Monad m, ToMarkedRow b, Entity a, ToField (EntityId a), Typeable a, Typeable b)
                 => (Query -> [Action] -> m Int64)
                 -> (EntityId a)
                 -> Proxy a
                 -> b
                 -> m ()
someUpdateEntity actor eid a row =
    unless (null rowlist) $ do
        _ <- actor q fields
        return ()
  where
    fields = (map snd rowlist)
             ++ [toField eid]
    q = mconcat
        [ "UPDATE "
        , dquo $ tableName a
        , " SET "
        , qIntercalate ", "
          $ map (nameToQ . fst) rowlist
        , " WHERE id = ?" ]

    rowlist = checkMR $ toMarkedRow row
    checkMR r = if (S.isSubsetOf
                    (S.fromList $ map fst r)
                    (S.fromList $ fieldNames a))
                then r
                else error
                     $ "fields of " <> tbname
                     <> " are not subset of fields of " <> taname
    tbname = show $ typeRep (Proxy :: Proxy b)
    taname = show $ typeRep (Proxy :: Proxy a)
    nameToQ name = (dquo name) <> " = ?"

-- | Update entity using 'ToMarkedRow' instanced value. Requires 'Proxy' while
-- 'EntityId' is not a data type.
--
-- @
-- fixVovka :: EntityId User -> Handler ()
-- fixVovka uid = do
--     pgGetEntity uid
--         >>= maybe notFound run
--   where
--     run user =
--         when ((userName user) == "Vovka")
--         $ pgUpdateEntity uid
--         (Proxy :: Proxy User)
--         [("active" :: Query, toField False)]
-- @
pgUpdateEntity :: forall a b m. (ToMarkedRow b, Entity a, HasPostgres m,
                           ToField (EntityId a), Functor m, Typeable a, Typeable b)
               => EntityId a
               -> Proxy a
               -> b
               -> m ()
pgUpdateEntity eid a row = someUpdateEntity pgExecute eid a row

mUpdateEntity :: forall a b m. (ToMarkedRow b, Entity a, PgMonad m,
                          ToField (EntityId a), Functor m, Typeable a, Typeable b)
              => EntityId a
              -> Proxy a
              -> b
              -> m ()
mUpdateEntity eid a row = someUpdateEntity mExecute eid a row

-- someSelectCount :: Proxy a -> Query -> prms -> m Integer

-- | Select count of entities with given query
--
-- @
-- activeUsers :: Handler Integer
-- activeUsers = do
--     pgSelectCount (Proxy :: Proxy User)
--         "WHERE active = ?" [True]
-- @
pgSelectCount :: forall m a prm. (Entity a, HasPostgres m, ToRow prm, Functor m)
              => Proxy a
              -> Query
              -> prm
              -> m Integer
pgSelectCount q prm = someSelectCount pgQuery q prm


mSelectCount :: forall m a prm. (Entity a, PgMonad m, ToRow prm, Functor m)
             => Proxy a
             -> Query
             -> prm
             -> m Integer
mSelectCount q prm = someSelectCount mQuery q prm

someSelectCount :: forall m a prm. (Entity a, Functor m)
                => (Query -> prm -> m [[Integer]])
                -> Proxy a
                -> Query
                -> prm
                -> m Integer
someSelectCount actor prox q prms =
    fstfst <$> actor selectQ prms
  where
    fstfst [(a:_)] = a
    fstfst _ = error
               "someSelectCount: query returned invalid count of values"
    selectQ =
        mconcat
        [ "SELECT COUNT(1) FROM "
        , tableName prox
        , " "
        , q ]
