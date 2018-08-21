module Database.PostgreSQL.Query.Entity.Functions
  ( -- * Work with entities
    pgInsertEntity
  , pgInsertManyEntities
  , pgInsertManyEntitiesId
  , pgSelectEntities
  , pgSelectJustEntities
  , pgSelectEntitiesBy
  , pgGetEntity
  , pgGetEntityBy
  , pgQueryEntities
  , pgDeleteEntity
  , pgUpdateEntity
  , pgSelectCount
  ) where

import Data.Int ( Int64 )
import Database.PostgreSQL.Query.Entity.Class
import Database.PostgreSQL.Query.Entity.Internal
import Database.PostgreSQL.Query.Functions
import Database.PostgreSQL.Query.Import
import Database.PostgreSQL.Query.SqlBuilder
import Database.PostgreSQL.Query.TH
    ( sqlExp )
import Database.PostgreSQL.Query.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import qualified Data.List as L
import qualified Data.List.NonEmpty as NL


-- | Insert new entity and return it's id
pgInsertEntity
  :: forall a m
   . ( MonadPostgres m, MonadLogger m, Entity a
     , ToRow a, FromField (EntityId a) )
  => a
  -> m (EntityId a)
pgInsertEntity a = do
    pgQuery [sqlExp|^{insertEntity a} RETURNING id|] >>= \case
        ((Only ret):_) -> return ret
        _       -> fail "Query did not return any response"


{- | Select entities as pairs of (id, entity).

@
handler :: Handler [Ent a]
handler = do
    now <- liftIO getCurrentTime
    let back = addUTCTime (days  (-7)) now
    pgSelectEntities id
        [sqlExp|WHERE created BETWEEN \#{now} AND \#{back}
               ORDER BY created|]

handler2 :: Text -> Handler [Ent Foo]
handler2 fvalue = do
    pgSelectEntities ("t"<>)
        [sqlExp|AS t INNER JOIN table2 AS t2
                ON t.t2_id = t2.id
                WHERE t.field = \#{fvalue}
                ORDER BY t2.field2|]
   -- Here the query will be: SELECT ... FROM tbl AS t INNER JOIN ...
@

-}

pgSelectEntities
  :: forall m a q
   . ( Functor m, MonadPostgres m, MonadLogger m, Entity a
     , FromRow a, ToSqlBuilder q, FromField (EntityId a) )
  => (FN -> FN)
   -- ^ Entity fields name modifier, e.g. ("tablename"<>). Each field of entity
   -- will be processed by this modifier before pasting to the query
  -> q
   -- ^ part of query just after __SELECT .. FROM table__.
  -> m [Ent a]
pgSelectEntities fpref q = do
    let p = Proxy :: Proxy a
    pgQueryEntities [sqlExp|^{selectEntity (entityFieldsId fpref) p} ^{q}|]


-- | Same as 'pgSelectEntities' but do not select id
pgSelectJustEntities
  :: forall m a q
   . ( Functor m, MonadPostgres m, MonadLogger m, Entity a
     , FromRow a, ToSqlBuilder q )
  => (FN -> FN)
  -> q
  -> m [a]
pgSelectJustEntities fpref q = do
    let p = Proxy :: Proxy a
    pgQuery [sqlExp|^{selectEntity (entityFields id fpref) p} ^{q}|]

{- | Select entities by condition formed from 'MarkedRow'. Usefull function when
you know

-}

pgSelectEntitiesBy
  :: forall a m b
   . ( Functor m, MonadPostgres m, MonadLogger m, Entity a, ToMarkedRow b
     , FromRow a, FromField (EntityId a) )
  => b
  -> m [Ent a]
pgSelectEntitiesBy b =
    let p = Proxy :: Proxy a
    in pgQueryEntities $ selectEntitiesBy ("id":) p b


-- | Select entity by id
--
-- @
-- getUser :: EntityId User ->  Handler User
-- getUser uid = do
--     pgGetEntity uid
--         >>= maybe notFound return
-- @
pgGetEntity
  :: forall m a
   . ( ToField (EntityId a), Entity a, FromRow a
     , MonadPostgres m, MonadLogger m, Functor m)
  => EntityId a
  -> m (Maybe a)
pgGetEntity eid = do
    listToMaybe <$> pgSelectJustEntities id [sqlExp|WHERE id = #{eid} LIMIT 1|]


{- | Get entity by some fields constraint

@
getUser :: UserName -> Handler User
getUser name = do
    pgGetEntityBy
        (MR [("name", mkValue name),
             ("active", mkValue True)])
        >>= maybe notFound return
@

The query here will be like

@
pgQuery [sqlExp|SELECT id, name, phone ... FROM users WHERE name = #{name} AND active = #{True}|]
@

-}

pgGetEntityBy
  :: forall m a b
   . ( Entity a, MonadPostgres m, MonadLogger m, ToMarkedRow b
     , FromField (EntityId a), FromRow a, Functor m )
  => b               -- ^ uniq constrained list of fields and values
  -> m (Maybe (Ent a))
pgGetEntityBy b =
    let p = Proxy :: Proxy a
    in fmap listToMaybe
       $ pgQueryEntities
       [sqlExp|^{selectEntitiesBy ("id":) p b} LIMIT 1|]


-- | Same as 'pgInsertEntity' but insert many entities at one
-- action. Returns list of id's of inserted entities
pgInsertManyEntitiesId
  :: forall a m
   . ( Entity a, MonadPostgres m, MonadLogger m
     , ToRow a, FromField (EntityId a))
  => [a]
  -> m [EntityId a]
pgInsertManyEntitiesId [] = return []
pgInsertManyEntitiesId ents' =
    let ents = NL.fromList ents'
        q = [sqlExp|^{insertManyEntities ents} RETURNING id|]
    in map fromOnly <$> pgQuery q

-- | Insert many entities without returning list of id like
-- 'pgInsertManyEntitiesId' does
pgInsertManyEntities
  :: forall a m
   . (Entity a, MonadPostgres m, MonadLogger m, ToRow a)
  => [a]
  -> m Int64
pgInsertManyEntities [] = return 0
pgInsertManyEntities ents' =
    let ents = NL.fromList ents'
    in pgExecute $ insertManyEntities ents


{- | Delete entity.

@
rmUser :: EntityId User -> Handler ()
rmUser uid = do
    pgDeleteEntity uid
@

Return 'True' if row was actually deleted.
-}

pgDeleteEntity
  :: forall a m
   . (Entity a, MonadPostgres m, MonadLogger m, ToField (EntityId a), Functor m)
  => EntityId a
  -> m Bool
pgDeleteEntity eid =
    let p = Proxy :: Proxy a
    in fmap (1 ==)
       $ pgExecute [sqlExp|DELETE FROM ^{tableName p}
                           WHERE id = #{eid}|]


{- | Update entity using 'ToMarkedRow' instanced value. Requires 'Proxy'
while 'EntityId' is not a data type.

@
fixUser :: Text -> EntityId User -> Handler ()
fixUser username uid = do
    pgGetEntity uid
        >>= maybe notFound run
  where
    run user =
        pgUpdateEntity uid
        $ MR [("active", mkValue True)
              ("name", mkValue username)]
@

Returns 'True' if record was actually updated and 'False' if there was
not row with such id (or was more than 1, in fact)
-}

pgUpdateEntity
  :: forall a b m
   . ( ToMarkedRow b, Entity a, MonadPostgres m, MonadLogger m
     , ToField (EntityId a), Functor m, Typeable a, Typeable b)
  => EntityId a
  -> b
  -> m Bool
pgUpdateEntity eid b =
    let p = Proxy :: Proxy a
        mr = toMarkedRow b
    in if L.null $ unMR mr
       then return False
       else fmap (1 ==)
            $ pgExecute [sqlExp|UPDATE ^{tableName p}
                                SET ^{mrToBuilder ", " mr}
                                WHERE id = #{eid}|]

{- | Select count of entities with given query

@
activeUsers :: Handler Integer
activeUsers = do
    pgSelectCount (Proxy :: Proxy User)
        [sqlExp|WHERE active = #{True}|]
@

-}


-- | Executes arbitrary query and parses it as entities and their ids
pgQueryEntities
  :: ( ToSqlBuilder q, MonadPostgres m, MonadLogger m, Entity a
     , FromRow a, FromField (EntityId a))
  => q
  -> m [Ent a]
pgQueryEntities q =
    map toTuples <$> pgQuery q
  where
    toTuples ((Only eid) :. entity) = (eid, entity)

pgSelectCount
  :: forall m a q
   . ( Entity a, MonadPostgres m, MonadLogger m, ToSqlBuilder q )
  => Proxy a
  -> q
  -> m Integer
pgSelectCount p q = do
    [[c]] <- pgQuery [sqlExp|SELECT count(id) FROM ^{tableName p} ^{q}|]
    return c
