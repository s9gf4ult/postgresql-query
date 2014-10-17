module PGSimple.Functions
       ( -- * One-query functions for 'HasPostgres' instances
         pgQuery
       , pgQuery_
       , pgReturning
       , pgExecute
       , pgExecute_
       , pgExecuteMany
         -- * Inserting entities
       , pgInsertEntity
       , mInsertEntity
       , pgInsertManyEntities
       , mInsertManyEntities
         -- * Selecting entities
       , pgSelectEntities
       , mSelectEntities
       , pgSelectJustEntities
       , mSelectJustEntities
       , pgGetEntity
       , mGetEntity
       , pgGetEntityBy
       , mGetEntityBy
         -- * Deleting entities
       , pgDeleteEntity
       , mDeleteEntity
         -- * Updating entities
       , pgUpdateEntity
       , mUpdateEntity
         -- * Counting entities
       , pgSelectCount
       , mSelectCount
       ) where

import Prelude

import Data.Int ( Int64 )
import Data.Proxy ( Proxy )
import Data.Typeable ( Typeable )
import Database.PostgreSQL.Simple as PG
    ( Query, ToRow, FromRow, execute_,
      returning, query_, query, executeMany, execute )
import Database.PostgreSQL.Simple.FromField
    ( FromField )
import Database.PostgreSQL.Simple.ToField
    ( ToField )

import PGSimple.Internal
import PGSimple.Types


pgQuery :: (HasPostgres m, ToRow q, FromRow r)
        => Query -> q -> m [r]
pgQuery q ps = withPostgres
               $ \c -> query c q ps

pgQuery_ :: (HasPostgres m, FromRow r)
         => Query -> m [r]
pgQuery_ q = withPostgres
             $ \c -> query_ c q

pgReturning :: (HasPostgres m, ToRow q, FromRow r)
            => Query -> [q] -> m [r]
pgReturning q ps = withPostgres
                   $ \c -> returning c q ps

pgExecute :: (HasPostgres m, ToRow q)
          => Query -> q -> m Int64
pgExecute q ps = withPostgres
                 $ \c -> execute c q ps

pgExecute_ :: (HasPostgres m)
           => Query -> m Int64
pgExecute_ q = withPostgres
               $ \c -> execute_ c q

pgExecuteMany :: (HasPostgres m, ToRow q)
              => Query -> [q] -> m Int64
pgExecuteMany q ps = withPostgres
                     $ \c -> executeMany c q ps



pgInsertEntity :: forall a m. (HasPostgres m, Entity a,
                         ToRow a, FromField (EntityId a))
               => a
               -> m (EntityId a)
pgInsertEntity a = someInsertEntity pgQuery a

mInsertEntity :: forall a m. (Entity a, PgMonad m, ToRow a, FromField (EntityId a))
              => a
              -> m (EntityId a)
mInsertEntity a = someInsertEntity mQuery a



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



-- | Same as 'pgInsertEntity' but insert many entities at on action
pgInsertManyEntities :: forall a m. (Entity a, HasPostgres m, ToRow a)
                     => [a]
                     -> m ()
pgInsertManyEntities a = someInsertManyEntities pgExecuteMany a


mInsertManyEntities :: forall a m. (Entity a, PgMonad m, ToRow a)
                    => [a]
                    -> m ()
mInsertManyEntities a = someInsertManyEntities mExecuteMany a



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
