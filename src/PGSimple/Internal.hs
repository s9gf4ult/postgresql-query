module PGSimple.Internal
       (
         FN(..)
       , MR(..)
       , ToMarkedRow(..)
       , mrToBuilder
       , mkIdent
       , mkValue
       , buildFields
       , entityFields
       , entityFieldsSimple
       , selectEntity
       , insertInto
       , insertEntity
       ) where


import Prelude

import Control.Applicative ( (<$>) )
import Control.Monad ( unless )
import Data.Maybe ( listToMaybe )
import Data.Monoid ( Monoid(..), (<>) )
import Data.Proxy ( Proxy(..) )
import Data.Text ( Text )
import Data.Typeable ( Typeable, typeRep )
import Database.PostgreSQL.Simple as PG
    ( Query, Only(Only), type (:.)(..) )
import Database.PostgreSQL.Simple.FromField ( FromField )
import Database.PostgreSQL.Simple.FromRow
    ( FromRow(..) )
import Database.PostgreSQL.Simple.ToField
    ( Action, ToField(..) )
import Database.PostgreSQL.Simple.ToRow
    ( ToRow(..) )
import Database.PostgreSQL.Simple.Types
    ( Query(..), Identifier(..) )
import GHC.Generics ( Generic )
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

-- | Data representing dot-separated field name
newtype FN =
    FN [Text]
    deriving (Ord, Eq, Show, Monoid, Typeable, Generic)

instance ToSqlBuilder FN where
    toSqlBuilder (FN tt) =
        mconcat
        $ L.intersperse "."
        $ map mkIdent tt

newtype MR =
    MR
    { unMR :: [(Text, Action)]
    } deriving (Show, Monoid, Typeable, Generic)

class ToMarkedRow a where
    -- | generate list of pairs (field name, field value)
    toMarkedRow :: a -> MR

instance ToMarkedRow MR where
    toMarkedRow = id

-- | Turns marked row to query condition or SET clause ih UPDATE query
-- e.g.
--
-- @
-- > mrToBuilder " AND " $ MR [(FN ["field"], toField 10), (FN ["field2"], toField 20)]
-- " \"field\" = 10  AND  \"field2\" = 20 "
-- @
mrToBuilder :: SqlBuilder        -- ^ Builder to intersperse with
            -> MR
            -> SqlBuilder
mrToBuilder b (MR l) = mconcat
                       $ L.intersperse b
                       $ map tobld l
  where
    tobld (f, val) = [sqlExp| ^{mkIdent f} = #{val} |]

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


insertInto :: (ToMarkedRow b)
           => Text               -- ^ table name
           -> b                  -- ^ list of pairs (name, value)
           -> SqlBuilder
insertInto tname b =
    let mr = toMarkedRow b
        names = mconcat
                $ L.intersperse ", "
                $ map (mkIdent . fst)
                $ unMR mr
        values = mconcat
                 $ L.intersperse ", "
                 $ map (mkValue . snd)
                 $ unMR mr
    in [sqlExp|INSERT INTO ^{mkIdent tname}
               (^{names}) VALUES (^{values})|]


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
