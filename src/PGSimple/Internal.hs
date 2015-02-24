module PGSimple.Internal where


import Prelude

import Data.Monoid
import Data.Proxy ( Proxy(..) )
import Data.Text ( Text )
import Database.PostgreSQL.Simple.ToRow
    ( ToRow(..) )
import PGSimple.SqlBuilder
    ( SqlBuilder, ToSqlBuilder(..),
      mkIdent, mkValue )
import PGSimple.TH
    ( sqlExp )
import PGSimple.Types
    ( FN(..), textFN, MarkedRow(..),
      ToMarkedRow(..), mrToBuilder )
import PGSimple.Entity
    ( Entity(..) )

import qualified Data.List as L

{- $setup
>>> import Database.PostgreSQL.Simple
>>> import PGSimple.SqlBuilder
>>> con <- connect defaultConnectInfo
-}


{-| Generates comma separated list of field names

>>> runSqlBuilder con $ buildFields ["u" <> "name", "u" <> "phone", "e" <> "email"]
"\"u\".\"name\", \"u\".\"phone\", \"e\".\"email\""
-}
buildFields :: [FN] -> SqlBuilder
buildFields flds = mconcat
                    $ L.intersperse ", "
                    $ map toSqlBuilder flds

{- | Build entity fields

>>> data Foo = Foo { fName :: Text, fSize :: Int }
>>> instance Entity Foo where {newtype EntityId Foo = FooId Int ; fieldNames _ = ["name", "size"] ; tableName _ = "foo"}
>>> runSqlBuilder con $ entityFields id id (Proxy :: Proxy Foo)
"\"name\", \"size\""

>>> runSqlBuilder con $ entityFields ("id":) id (Proxy :: Proxy Foo)
"\"id\", \"name\", \"size\""

>>> runSqlBuilder con $ entityFields (\l -> ("id":l) ++ ["created"]) id (Proxy :: Proxy Foo)
"\"id\", \"name\", \"size\", \"created\""

>>> runSqlBuilder con $ entityFields id ("f"<>) (Proxy :: Proxy Foo)
"\"f\".\"name\", \"f\".\"size\""

>>> runSqlBuilder con $ entityFields ("f.id":) ("f"<>) (Proxy :: Proxy Foo)
"\"f\".\"id\", \"f\".\"name\", \"f\".\"size\""

-}

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

{- | Same as 'entityFields' but prefixes list of names with __id__
field. This is shorthand function for often usage.

>>> data Foo = Foo { fName :: Text, fSize :: Int }
>>> instance Entity Foo where {newtype EntityId Foo = FooId Int ; fieldNames _ = ["name", "size"] ; tableName _ = "foo"}
>>> runSqlBuilder con $ entityFieldsId id (Proxy :: Proxy Foo)
"\"id\", \"name\", \"size\""

>>> runSqlBuilder con $ entityFieldsId ("f"<>) (Proxy :: Proxy Foo)
"\"f\".\"id\", \"f\".\"name\", \"f\".\"size\""

-}

entityFieldsId :: (Entity a)
               => (FN -> FN)
               -> Proxy a
               -> SqlBuilder
entityFieldsId fpref p =
    let xpref = ((fpref "id"):)
    in entityFields xpref fpref p

{- | Generate SELECT query string for entity

>>> data Foo = Foo { fName :: Text, fSize :: Int }
>>> instance Entity Foo where {newtype EntityId Foo = FooId Int ; fieldNames _ = ["name", "size"] ; tableName _ = "foo"}
>>> runSqlBuilder con $ selectEntity (entityFieldsId id) (Proxy :: Proxy Foo)
"SELECT \"id\", \"name\", \"size\" FROM \"foo\""

>>> runSqlBuilder con $ selectEntity (entityFieldsId ("f"<>)) (Proxy :: Proxy Foo)
"SELECT \"f\".\"id\", \"f\".\"name\", \"f\".\"size\" FROM \"foo\""

>>> runSqlBuilder con $ selectEntity (entityFields id id) (Proxy :: Proxy Foo)
"SELECT \"name\", \"size\" FROM \"foo\""

-}

selectEntity :: (Entity a)
             => (Proxy a -> SqlBuilder) -- ^ build fields part from proxy
             -> Proxy a
             -> SqlBuilder
selectEntity bld p =
    [sqlExp|SELECT ^{bld p} FROM ^{mkIdent $ tableName p}|]


{- | Generate INSERT INTO query for entity

>>> runSqlBuilder con $ insertInto "foo" $ MR [("name", mkValue "vovka"), ("hobby", mkValue "president")]
"INSERT INTO \"foo\" (\"name\", \"hobby\") VALUES ('vovka', 'president')"

-}

insertInto :: (ToMarkedRow b)
           => Text               -- ^ table name
           -> b                  -- ^ list of pairs (name, value) to insert into
           -> SqlBuilder
insertInto tname b =
    let mr = toMarkedRow b
        names = mconcat
                $ L.intersperse ", "
                $ map (toSqlBuilder . fst)
                $ unMR mr
        values = mconcat
                 $ L.intersperse ", "
                 $ map snd
                 $ unMR mr
    in [sqlExp|INSERT INTO ^{mkIdent tname}
               (^{names}) VALUES (^{values})|]

-- | Convert entity to marked row to perform inserts and anything else
entityToMR :: forall a. (Entity a, ToRow a) => a -> MarkedRow
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
