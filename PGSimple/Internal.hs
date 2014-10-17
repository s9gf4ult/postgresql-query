module PGSimple.Internal where


import Prelude

import Control.Applicative ( (<$>) )
import Control.Monad ( unless )
import Data.Int ( Int64 )
import Data.Maybe ( listToMaybe )
import Data.Monoid ( Monoid(mconcat), (<>) )
import Data.Proxy ( Proxy(..) )
import Data.Typeable ( Typeable, typeRep )
import Database.PostgreSQL.Simple as PG
    ( Query, Only(Only), type (:.)(..), ToRow, FromRow )
import Database.PostgreSQL.Simple.FromField ( FromField )
import Database.PostgreSQL.Simple.ToField
    ( Action, ToField(..) )
import Database.PostgreSQL.Simple.Types
    ( Query(..) )
import PGSimple.Types

import qualified Data.ByteString as BS
import qualified Data.Set as S



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
