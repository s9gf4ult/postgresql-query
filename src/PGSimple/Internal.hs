module PGSimple.Internal
       ( -- * Query generation helpers
         mkIdent
       , mkValue
       , buildFields
       , entityFields
       , entityFieldsSimple
       , selectEntity
       , insertEntity
       ) where


import Prelude

import Control.Applicative ( (<$>) )
import Control.Monad ( unless )
import Data.Maybe ( listToMaybe )
import Data.Monoid ( Monoid(mconcat), (<>) )
import Data.Proxy ( Proxy(..) )
import Data.Text ( Text )
import Data.Typeable ( Typeable, typeRep )
import Database.PostgreSQL.Simple as PG
    ( Query, Only(Only), type (:.)(..) )
import Database.PostgreSQL.Simple.ToRow
    ( ToRow(..) )
import Database.PostgreSQL.Simple.FromRow
    ( FromRow(..) )
import Database.PostgreSQL.Simple.FromField ( FromField )
import Database.PostgreSQL.Simple.ToField
    ( Action, ToField(..) )
import Database.PostgreSQL.Simple.Types
    ( Query(..), Identifier(..) )
import PGSimple.SqlBuilder
import PGSimple.TH
import PGSimple.Types

import qualified Data.ByteString as BS
import qualified Data.Set as S
import qualified Data.List as L


-- | Shorthand function to convert identifier name to builder
mkIdent :: Text -> SqlBuilder
mkIdent t = mkValue $ Identifier t

-- | Shorthand function to convert single value to builder
mkValue :: (ToField a) => a -> SqlBuilder
mkValue a = [sqlExp|#{a}|]


-- | Fields of entity separated with coma. Each nested list is a dot-separated
-- identifiers, so __[["t", "field"], ["t2", "field"]] -> "t"."field", "t2"."field"__
buildFields :: [[Text]] -> SqlBuilder
buildFields flds = mconcat
                    $ L.intersperse ", "
                    $ map toBld flds
  where
    toBld :: [Text] -> SqlBuilder
    toBld tt = mconcat
               $ L.intersperse "."
               $ map mkIdent tt

-- | Build entity fields
entityFields :: (Entity a)
             => ([[Text]] -> [[Text]])      -- ^ modify list of fields
             -> ([Text] -> [Text])          -- ^ modify each field name,
                                          -- e.g. prepend each field with
                                          -- prefix, like ("t":). Applied first
             -> Proxy a
             -> SqlBuilder
entityFields xpref fpref p =
    buildFields
    $ xpref
    $ map (fpref . (:[]))
    $ fieldNames p

entityFieldsSimple :: (Entity a)
                   => ([Text] -> [Text])
                   -> Proxy a
                   -> SqlBuilder
entityFieldsSimple fpref p =
    let xpref = ((fpref ["id"]):)
    in entityFields xpref fpref p

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
-- "SELECT \\"fld1\\", \\"fld2\\" FROM tbl"
-- λ> selectEntity False Nothing ("id":) (Proxy :: Proxy Tbl)
-- "SELECT \\"id\\", \\"fld1\\", \\"fld2\\" FROM tbl"
-- λ> selectEntity False (Just "t") ("id":) (Proxy :: Proxy Tbl)
-- "SELECT \\"t\\".\\"id\\", \\"t\\".\\"fld1\\", \\"t\\".\\"fld2\\" FROM tbl"
-- @
selectEntity :: (Entity a)
             => (Proxy a -> SqlBuilder) -- ^ build fields part from proxy
             -> Proxy a
             -> SqlBuilder
selectEntity bld p =
    [sqlExp|SELECT ^{bld p} FROM ^{mkIdent $ tableName p}|]



-- | Same as 'selectEntity' but generates INSERT query
insertEntity :: forall a. (Entity a, ToRow a) => a -> SqlBuilder
insertEntity a =
    let p = Proxy :: Proxy a
        names = buildFields
                $ map (:[])
                $ fieldNames p
        values = mconcat
                 $ L.intersperse ", "
                 $ map mkValue
                 $ toRow a
    in [sqlExp| INSERT INTO ^{mkIdent $ tableName p}
                (^{names}) VALUES (^{values}) |]




-- someGetEntity :: forall m a. (Functor m, Entity a, FromRow a, ToField (EntityId a))
--               => (Query -> Only (EntityId a) -> m [a])
--               -> EntityId a
--               -> m (Maybe a)
-- someGetEntity actor eid =
--     listToMaybe <$> actor selectQ (Only eid)
--   where
--     selectQ =
--         mconcat
--         [ selectEntity False Nothing id
--           (Proxy :: Proxy a)
--         , "  WHERE id = ? LIMIT 1 " ]


-- someGetEntityBy :: forall m a b. (Functor m, Entity a, FromRow a,
--                             FromField (EntityId a), ToMarkedRow b)
--                 => (Query -> [Action] -> m [(Only (EntityId a)) :. a])
--                 -> b             -- ^ constrain row
--                 -> m (Maybe (Ent a))
-- someGetEntityBy actor row =
--     ((fmap toTuple) . listToMaybe)
--     <$> actor selectQ (map snd fields)
--   where
--     fields = toMarkedRow row
--     toTuple ((Only eid) :. e) = (eid, e)
--     selectQ =
--         mconcat
--         [ selectEntity False Nothing ("id":)
--           (Proxy :: Proxy a)
--         , " WHERE "
--         , conditions
--         , " LIMIT 1" ]
--     conditions = qIntercalate " AND "
--                  $ map ((\f -> (dquo f) <> " = ?") . fst)
--                  fields


-- someInsertManyEntities :: forall a m x. (Monad m, Entity a, ToRow a)
--                        => (Query -> [a] -> m x) -- ^ query executor
--                        -> [a]                     -- ^ entity
--                        -> m ()
-- someInsertManyEntities actor a = do
--     _ <- actor (insertEntity (Proxy :: Proxy a)) a
--     return ()


-- someDeleteEntity :: forall a m x. (Entity a, ToField (EntityId a), Functor m)
--                  => (Query -> Only (EntityId a) -> m x)
--                  -> EntityId a
--                  -> m ()
-- someDeleteEntity actor eid = fmap (const ())
--                              $ actor q
--                              $ Only eid
--   where
--     q = mconcat
--         [ "DELETE FROM "
--         , dquo $ tableName (Proxy :: Proxy a)
--         , " WHERE id = ? " ]


-- someUpdateEntity :: forall a b m x. (Monad m, ToMarkedRow b, Entity a, ToField (EntityId a), Typeable a, Typeable b)
--                  => (Query -> [Action] -> m x)
--                  -> (EntityId a)
--                  -> b
--                  -> m ()
-- someUpdateEntity actor eid prm =
--     unless (null rowlist) $ do
--         _ <- actor q fields
--         return ()
--   where
--     fields = (map snd rowlist)
--              ++ [toField eid]
--     q = mconcat
--         [ "UPDATE "
--         , dquo $ tableName (Proxy :: Proxy a)
--         , " SET "
--         , qIntercalate ", "
--           $ map (nameToQ . fst) rowlist
--         , " WHERE id = ?" ]

--     rowlist = checkMR $ toMarkedRow prm
--     checkMR r = if (S.isSubsetOf
--                     (S.fromList $ map fst r)
--                     (S.fromList $ fieldNames
--                      (Proxy :: Proxy a)))
--                 then r
--                 else error
--                      $ "fields of " <> tbname
--                      <> " are not subset of fields of " <> taname
--     tbname = show $ typeRep (Proxy :: Proxy b)
--     taname = show $ typeRep (Proxy :: Proxy a)
--     nameToQ name = (dquo name) <> " = ?"


-- someSelectCount :: forall m a prm. (Entity a, Functor m)
--                 => (Query -> prm -> m [[Integer]])
--                 -> Proxy a
--                 -> Query
--                 -> prm
--                 -> m Integer
-- someSelectCount actor prox q prms =
--     fstfst <$> actor selectQ prms
--   where
--     fstfst [(a:_)] = a
--     fstfst _ = error
--                "someSelectCount: query returned invalid count of values"
--     selectQ =
--         mconcat
--         [ "SELECT COUNT(1) FROM "
--         , tableName prox
--         , " "
--         , q ]
