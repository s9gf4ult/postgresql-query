module Database.PostgreSQL.Query.Internal
       ( -- * Low level generators
         buildFields
       , updateTable
       , insertInto
       ) where

import Database.PostgreSQL.Query.SqlBuilder
import Database.PostgreSQL.Query.TH
import Database.PostgreSQL.Query.Types

import qualified Data.List as L

{- $setup
>>> import Database.PostgreSQL.Simple
>>> import Database.PostgreSQL.Simple.ToField
>>> import Database.PostgreSQL.Query.SqlBuilder
>>> import Data.Text ( Text )
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

{- | generates __UPDATE__ query

>>> let name = "%vip%"
>>> runSqlBuilder con $ updateTable "ships" (MR [("size", mkValue 15)]) [sqlExp|WHERE size > 15 AND name NOT LIKE #{name}|]
"UPDATE \"ships\" SET  \"size\" = 15  WHERE size > 15 AND name NOT LIKE '%vip%'"

-}

updateTable :: (ToSqlBuilder q, ToMarkedRow flds)
            => FN               -- ^ table name
            -> flds             -- ^ fields to update
            -> q                -- ^ condition
            -> SqlBuilder
updateTable tname flds q =
    let mr = toMarkedRow flds
        setFields = mrToBuilder ", " mr
    in [sqlExp|UPDATE ^{tname}
               SET ^{setFields} ^{q}|]


{- | Generate INSERT INTO query for entity

>>> runSqlBuilder con $ insertInto "foo" $ MR [("name", mkValue "vovka"), ("hobby", mkValue "president")]
"INSERT INTO \"foo\" (\"name\", \"hobby\") VALUES ('vovka', 'president')"

-}

insertInto :: (ToMarkedRow b)
           => FN       -- ^ table name
           -> b        -- ^ list of pairs (name, value) to insert into
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
    in [sqlExp|INSERT INTO ^{tname}
               (^{names}) VALUES (^{values})|]
