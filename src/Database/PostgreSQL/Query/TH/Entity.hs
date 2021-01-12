module Database.PostgreSQL.Query.TH.Entity
  ( EntityOptions(..)
  , deriveEntity
  ) where

import Data.Default
import Database.PostgreSQL.Query.Entity.Class
import Database.PostgreSQL.Query.Import
import Database.PostgreSQL.Query.TH.Common
import Database.PostgreSQL.Query.Types ( FN(..), textFN )
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.Inflections

import qualified Data.Text as T

-- | Options for deriving `Entity`
data EntityOptions = EntityOptions
    { eoTableName      :: Text -> FN -- ^ Type name to table name converter
    , eoColumnNames    :: Text -> FN -- ^ Record field to column name converter
    , eoDeriveClasses  :: [Name]     -- ^ Typeclasses to derive for Id
    , eoIdType         :: Name       -- ^ Base type for Id
    } deriving (Generic)

#if !MIN_VERSION_inflections(0,3,0)
toUnderscore' :: Text -> Text
toUnderscore' = T.pack . toUnderscore . T.unpack
#else
toUnderscore' :: Text -> Text
toUnderscore' = either error' id . toUnderscore
  where
    error' er = error $ "toUnderscore: " ++ show er
#endif

instance Default EntityOptions where
  def = EntityOptions
        { eoTableName     = textFN . toUnderscore'
        , eoColumnNames   = textFN . toUnderscore'
        , eoDeriveClasses = [ ''Ord, ''Eq, ''Show
                            , ''FromField, ''ToField ]
        , eoIdType        = ''Integer
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
      , eoTableName     = textFN . toUnderscore'
      , eoColumnNames   = textFN . toUnderscore' . drop 1
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
    tcon <- dataConstructors <$> reify tname >>= \case
      [a] -> return a
      x -> fail $ "expected exactly 1 data constructor, but " ++ show (length x) ++ " got"
    econt <- [t|Entity $(conT tname)|]
    eidcont <- [t|EntityId $(conT tname)|]
    ConT entityIdName <- [t|EntityId|]
    let tnames = nameBase tname
        idname = tnames ++ "Id"
        unidname = "get" ++ idname
        idtype = ConT (eoIdType opts)
#if MIN_VERSION_template_haskell(2,15,0)
        idcon = RecC (mkName idname)
                [(mkName unidname, Bang NoSourceUnpackedness NoSourceStrictness, idtype)]
        iddec = NewtypeInstD [] Nothing eidcont Nothing
                idcon [DerivClause Nothing (map ConT $ eoDeriveClasses opts)]
#elif MIN_VERSION_template_haskell(2,12,0)
        idcon = RecC (mkName idname)
                [(mkName unidname, Bang NoSourceUnpackedness NoSourceStrictness, idtype)]
        iddec = NewtypeInstD [] entityIdName [ConT tname] Nothing
                idcon [DerivClause Nothing (map ConT $ eoDeriveClasses opts)]
#elif MIN_VERSION_template_haskell(2,11,0)
        idcon = RecC (mkName idname)
                [(mkName unidname, Bang NoSourceUnpackedness NoSourceStrictness, idtype)]
        iddec = NewtypeInstD [] entityIdName [ConT tname] Nothing
                idcon (map ConT $ eoDeriveClasses opts)
#else
        idcon = RecC (mkName idname)
                [(mkName unidname, NotStrict, idtype)]
        iddec = NewtypeInstD [] entityIdName [ConT tname]
                idcon (eoDeriveClasses opts)
#endif
        tblName = eoTableName opts $ T.pack tnames
        fldNames = map (eoColumnNames opts . T.pack . nameBase)
                   $ cFieldNames tcon
    VarE ntableName  <- [e|tableName|]
    VarE nfieldNames <- [e|fieldNames|]
    tblExp <- lift (tblName :: FN)
    fldExp <- mapM lift (fldNames :: [FN])
    let tbldec = FunD ntableName  [Clause [WildP] (NormalB tblExp) []]
        flddec = FunD nfieldNames [Clause [WildP] (NormalB $ ListE fldExp) []]
#if MIN_VERSION_template_haskell(2,11,0)
        ret = InstanceD Nothing [] econt [ iddec, tbldec, flddec ]
#else
        ret = InstanceD [] econt [ iddec, tbldec, flddec ]
#endif
        syndec = TySynD (mkName idname) [] (AppT (ConT entityIdName) (ConT tname))
    return [ret, syndec]
