module Database.PostgreSQL.Query.TH.Entity where

import Prelude

import Database.PostgreSQL.Query.TH.Enum

import Control.Applicative
import Control.Monad
import Data.Default
import Data.FileEmbed ( embedFile )
import Data.String
import Database.PostgreSQL.Query.Entity ( Entity(..) )
import Database.PostgreSQL.Query.TH.Common
import Database.PostgreSQL.Query.Types ( FN(..) )
import Database.PostgreSQL.Simple.FromRow ( FromRow(..), field )
import Database.PostgreSQL.Simple.ToRow ( ToRow(..) )
import Database.PostgreSQL.Simple.Types ( Query(..) )
import Language.Haskell.TH
import Language.Haskell.TH.Syntax


-- | Options for deriving `Entity`
data EntityOptions = EntityOptions
    { eoTableName      :: String -> String -- ^ Type name to table name converter
    , eoColumnNames    :: String -> String -- ^ Record field to column name converter
    , eoDeriveClasses  :: [Name]           -- ^ Typeclasses to derive for Id
    , eoIdType         :: Name             -- ^ Base type for Id
    }

instance Default EntityOptions where
    def = EntityOptions
        { eoTableName = id
        , eoColumnNames = id
                          --  FIXME: ++ FromField, ToField
        , eoDeriveClasses = [''Ord, ''Eq, ''Show]
        , eoIdType = ''Integer
        }

{- | Derives instance for 'Entity' using type name and field names. Also
generates type synonim for ID. E.g. code like this:

@
data Agent = Agent
    { aName          :: !Text
    , aAttributes    :: !HStoreMap
    , aLongWeirdName :: !Int
    } deriving (Ord, Eq, Show)

$(deriveEntity
  def { eoIdType        = ''Id
      , eoTableName     = toUnderscore
      , eoColumnNames   = toUnderscore . drop 1
      , eoDeriveClasses =
        [''Show, ''Read, ''Ord, ''Eq
        , ''FromField, ''ToField, ''PathPiece]
      }
  ''Agent )
@

Will generate code like this:

@
instance Database.PostgreSQL.Query.Entity Agent where
    newtype EntityId Agent
        = AgentId {getAgentId :: Id}
        deriving (Show, Read, Ord, Eq, FromField, ToField, PathPiece)
    tableName _ = "agent"
    fieldNames _ = ["name", "attributes", "long_weird_name"]
type AgentId = EntityId Agent
@

So, you dont need to write it by hands any more.

NOTE: 'toUnderscore' is from package 'inflections' here
-}

deriveEntity :: EntityOptions -> Name -> Q [Dec]
deriveEntity opts tname = do
    TyConI (DataD _ _ _ [tcon] _) <- reify tname
    econt <- [t|Entity $(conT tname)|]
    ConT entityIdName <- [t|EntityId|]
    let tnames = nameBase tname
        idname = tnames ++ "Id"
        unidname = "get" ++ idname
        idtype = ConT (eoIdType opts)
        idcon = RecC (mkName idname)
                [(mkName unidname, NotStrict, idtype)]
        iddec = NewtypeInstD [] entityIdName [ConT tname]
                idcon (eoDeriveClasses opts)
        tblName = fromString $ eoTableName opts tnames
        fldNames = map (fromString . eoColumnNames opts . nameBase)
                   $ cFieldNames tcon
    VarE ntableName  <- [e|tableName|]
    VarE nfieldNames <- [e|fieldNames|]
    tblExp <- lift (tblName :: FN)
    fldExp <- mapM lift (fldNames :: [FN])
    let tbldec = FunD ntableName  [Clause [WildP] (NormalB tblExp) []]
        flddec = FunD nfieldNames [Clause [WildP] (NormalB $ ListE fldExp) []]
        ret = InstanceD [] econt
              [ iddec, tbldec, flddec ]
        syndec = TySynD (mkName idname) [] (AppT (ConT entityIdName) (ConT tname))
    return [ret, syndec]
