module Database.PostgreSQL.Query.Entity.Internal
  ( -- * Entity functions
    entityFields
  , entityFieldsId
  , selectEntity
  , selectEntitiesBy
  , insertEntity
  , insertManyEntities
  , entityToMR
  ) where

import Database.PostgreSQL.Query.Entity.Class
import Database.PostgreSQL.Query.Import
import Database.PostgreSQL.Query.Internal
import Database.PostgreSQL.Query.SqlBuilder
    ( SqlBuilder, ToSqlBuilder(..), mkValue )
import Database.PostgreSQL.Query.TH
    ( sqlExp )
import Database.PostgreSQL.Query.Types
    ( FN(..), MarkedRow(..),
      ToMarkedRow(..), mrToBuilder )
import Database.PostgreSQL.Simple.ToRow
    ( ToRow(..) )

import qualified Data.List.NonEmpty as NL
import qualified Data.List as L

{- $setup
>>> import Database.PostgreSQL.Simple
>>> import Database.PostgreSQL.Simple.ToField
>>> import Database.PostgreSQL.Query.SqlBuilder
>>> con <- connect defaultConnectInfo
>>> run b = fmap fst $ runSqlBuilder con defaultLogMasker b
-}


{- | Build entity fields

>>> data Foo = Foo { fName :: Text, fSize :: Int }
>>> instance Entity Foo where {newtype EntityId Foo = FooId Int ; fieldNames _ = ["name", "size"] ; tableName _ = "foo"}
>>> run $ entityFields id id (Proxy :: Proxy Foo)
"\"name\", \"size\""

>>> run $ entityFields ("id":) id (Proxy :: Proxy Foo)
"\"id\", \"name\", \"size\""

>>> run $ entityFields (\l -> ("id":l) ++ ["created"]) id (Proxy :: Proxy Foo)
"\"id\", \"name\", \"size\", \"created\""

>>> run $ entityFields id ("f"<>) (Proxy :: Proxy Foo)
"\"f\".\"name\", \"f\".\"size\""

>>> run $ entityFields ("f.id":) ("f"<>) (Proxy :: Proxy Foo)
"\"f\".\"id\", \"f\".\"name\", \"f\".\"size\""

-}

entityFields :: (Entity a)
             => ([FN] -> [FN])    -- ^ modify list of fields. Applied second
             -> (FN -> FN)        -- ^ modify each field name,
                                -- e.g. prepend each field with
                                -- prefix, like ("t"<>). Applied first
             -> Proxy a
             -> SqlBuilder
entityFields xpref fpref p =
    buildFields
    $ xpref
    $ map fpref
    $ fieldNames p

{- | Same as 'entityFields' but prefixes list of names with __id__
field. This is shorthand function for often usage.

>>> data Foo = Foo { fName :: Text, fSize :: Int }
>>> instance Entity Foo where {newtype EntityId Foo = FooId Int ; fieldNames _ = ["name", "size"] ; tableName _ = "foo"}
>>> run $ entityFieldsId id (Proxy :: Proxy Foo)
"\"id\", \"name\", \"size\""

>>> run $ entityFieldsId ("f"<>) (Proxy :: Proxy Foo)
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
>>> run $ selectEntity (entityFieldsId id) (Proxy :: Proxy Foo)
"SELECT \"id\", \"name\", \"size\" FROM \"foo\""

>>> run $ selectEntity (entityFieldsId ("f"<>)) (Proxy :: Proxy Foo)
"SELECT \"f\".\"id\", \"f\".\"name\", \"f\".\"size\" FROM \"foo\""

>>> run $ selectEntity (entityFields id id) (Proxy :: Proxy Foo)
"SELECT \"name\", \"size\" FROM \"foo\""

-}

selectEntity :: (Entity a)
             => (Proxy a -> SqlBuilder) -- ^ build fields part from proxy
             -> Proxy a
             -> SqlBuilder
selectEntity bld p =
    [sqlExp|SELECT ^{bld p} FROM ^{tableName p}|]


{- | Generates SELECT FROM WHERE query with most used conditions

>>> data Foo = Foo { fName :: Text, fSize :: Int }
>>> instance Entity Foo where {newtype EntityId Foo = FooId Int ; fieldNames _ = ["name", "size"] ; tableName _ = "foo"}
>>> run $ selectEntitiesBy id (Proxy :: Proxy Foo) $ MR []
"SELECT \"name\", \"size\" FROM \"foo\""

>>> run $ selectEntitiesBy id (Proxy :: Proxy Foo) $ MR [("name", mkValue "fooname")]
"SELECT \"name\", \"size\" FROM \"foo\" WHERE  \"name\" = 'fooname' "

>>> run $ selectEntitiesBy id (Proxy :: Proxy Foo) $ MR [("name", mkValue "fooname"), ("size", mkValue 10)]
"SELECT \"name\", \"size\" FROM \"foo\" WHERE  \"name\" = 'fooname' AND \"size\" = 10 "

-}

selectEntitiesBy :: (Entity a, ToMarkedRow b)
                 => ([FN] -> [FN])
                 -> Proxy a
                 -> b
                 -> SqlBuilder
selectEntitiesBy xpref p b =
    let mr = toMarkedRow b
        cond = if L.null $ unMR mr
               then mempty
               else [sqlExp| WHERE ^{mrToBuilder "AND" mr}|]
        q = selectEntity (entityFields xpref id) p
    in q <> cond



{- | Convert entity instance to marked row to perform inserts updates
and same stuff

>>> data Foo = Foo { fName :: Text, fSize :: Int }
>>> instance Entity Foo where {newtype EntityId Foo = FooId Int ; fieldNames _ = ["name", "size"] ; tableName _ = "foo"}
>>> instance ToRow Foo where { toRow Foo{..} = [toField fName, toField fSize] }
>>> run $ mrToBuilder ", " $ entityToMR $ Foo "Enterprise" 610
" \"name\" = 'Enterprise' ,  \"size\" = 610 "

-}

entityToMR :: forall a. (Entity a, ToRow a) => a -> MarkedRow
entityToMR a =
    let p = Proxy :: Proxy a
        names = fieldNames p
        values = map mkValue $ toRow a
    in MR $ zip names values


{- | Generates __INSERT INTO__ query for any instance of 'Entity' and 'ToRow'

>>> data Foo = Foo { fName :: Text, fSize :: Int }
>>> instance Entity Foo where {newtype EntityId Foo = FooId Int ; fieldNames _ = ["name", "size"] ; tableName _ = "foo"}
>>> instance ToRow Foo where { toRow Foo{..} = [toField fName, toField fSize] }
>>> run $ insertEntity $ Foo "Enterprise" 910
"INSERT INTO \"foo\" (\"name\", \"size\") VALUES ('Enterprise', 910)"

-}

insertEntity :: forall a. (Entity a, ToRow a) => a -> SqlBuilder
insertEntity a =
    let p = Proxy :: Proxy a
        mr = entityToMR a
    in insertInto (tableName p) mr

{- | Same as 'insertEntity' but generates query to insert many queries at same time

>>> import Database.PostgreSQL.Simple.ToField
>>> import Database.PostgreSQL.Query.SqlBuilder.Builder
>>> data Foo = Foo { fName :: Text, fSize :: Int }
>>> instance Entity Foo where {newtype EntityId Foo = FooId Int ; fieldNames _ = ["name", "size"] ; tableName _ = "foo"}
>>> instance ToRow Foo where { toRow Foo{..} = [toField fName, toField fSize] }
>>> run $ insertManyEntities $ NL.fromList [Foo "meter" 1, Foo "table" 2, Foo "earth" 151930000000]
"INSERT INTO \"foo\" (\"name\",\"size\") VALUES ('meter',1),('table',2),('earth',151930000000)"

-}

insertManyEntities :: forall a. (Entity a, ToRow a)
                   => NonEmpty a
                   -> SqlBuilder
insertManyEntities rows =
    let p = Proxy :: Proxy a
        names = mconcat
                $ L.intersperse ","
                $ map toSqlBuilder
                $ fieldNames p
        values = mconcat
                 $ L.intersperse ","
                 $ map rValue
                 $ NL.toList rows

    in [sqlExp|INSERT INTO ^{tableName p}
               (^{names}) VALUES ^{values}|]
  where
    rValue :: a -> SqlBuilder
    rValue row =
        let values = mconcat
                     $ L.intersperse ","
                     $ map mkValue
                     $ toRow row
        in [sqlExp|(^{values})|]
