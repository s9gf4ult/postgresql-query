module PGSimple.Internal where


import Prelude

import Data.Monoid
import Data.Proxy ( Proxy(..) )
import Data.Text ( Text )
import Database.PostgreSQL.Simple.ToRow
    ( ToRow(..) )
import PGSimple.SqlBuilder
import PGSimple.TH
import PGSimple.Types

import qualified Data.List as L


-- | Generate fields separated by comma.
buildFields :: [FN] -> SqlBuilder
buildFields flds = mconcat
                    $ L.intersperse ", "
                    $ map toSqlBuilder flds

-- | Build entity fields
entityFields :: (Entity a)
             => ([FN] -> [FN])    -- ^ modify list of fields
             -> (FN -> FN)        -- ^ modify each field name,
                                -- e.g. prepend each field with
                                -- prefix, like ("t":). Applied first
             -> Proxy a
             -> SqlBuilder
entityFields xpref fpref p =
    buildFields
    $ xpref
    $ map (fpref . FN . (:[]))
    $ fieldNames p

entityFieldsSimple :: (Entity a)
                   => (FN -> FN)
                   -> Proxy a
                   -> SqlBuilder
entityFieldsSimple fpref p =
    let xpref = ((fpref "id"):)
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


-- | Generate __INSERT INTO__ query
insertInto :: (ToMarkedRow b)
           => Text               -- ^ table name
           -> b                  -- ^ list of pairs (name, value) to insert into
           -> SqlBuilder
insertInto tname b =
    let mr = toMarkedRow b
        names = mconcat
                $ L.intersperse ", "
                $ map fst
                $ unMR mr
        values = mconcat
                 $ L.intersperse ", "
                 $ map snd
                 $ unMR mr
    in [sqlExp|INSERT INTO ^{mkIdent tname}
               (^{names}) VALUES (^{values})|]

-- | Convert entity to marked row to perform inserts and anything else
entityToMR :: forall a. (Entity a, ToRow a) => a -> MR
entityToMR a =
    let p = Proxy :: Proxy a
        names = map textFN $ fieldNames p
        values = map mkValue $ toRow a
    in MR $ zip names values


-- | Same as 'selectEntity' but generates INSERT query
insertEntity :: forall a. (Entity a, ToRow a) => a -> SqlBuilder
insertEntity a =
    let p = Proxy :: Proxy a
        mr = entityToMR a
    in insertInto (tableName p) mr


updateTable :: (ToSqlBuilder q, ToMarkedRow flds)
            => Text              -- ^ table name
            -> flds              -- ^ fields to update
            -> q                 -- ^ condition
            -> SqlBuilder
updateTable tname flds q =
    let mr = toMarkedRow flds
        setFields = mrToBuilder ", " mr
    in [sqlExp|UPDATE ^{mkIdent tname}
               SET ^{setFields} ^{q}|]


insertManyEntities :: forall a. (Entity a, ToRow a) => [a] -> SqlBuilder
insertManyEntities rows =
    let p = Proxy :: Proxy a
        names = mconcat
                $ L.intersperse ","
                $ map mkIdent
                $ fieldNames p
        values = mconcat
                 $ L.intersperse ","
                 $ map rValue rows
    in [sqlExp|INSERT INTO ^{mkIdent $ tableName p}
               (^{names}) VALUES ^{values}|]
  where
    rValue row =
        let values = mconcat
                     $ L.intersperse ","
                     $ map mkValue
                     $ toRow row
        in [sqlExp|(^{values})|]
