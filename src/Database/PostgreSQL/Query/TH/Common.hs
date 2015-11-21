module Database.PostgreSQL.Query.TH.Common where

import Prelude

import Control.Applicative
import Control.Monad
import Data.Default
import Data.FileEmbed ( embedFile )
import Data.String
import Database.PostgreSQL.Query.Entity ( Entity(..) )
import Database.PostgreSQL.Query.Types ( FN(..) )
import Database.PostgreSQL.Simple.FromRow ( FromRow(..), field )
import Database.PostgreSQL.Simple.ToRow ( ToRow(..) )
import Database.PostgreSQL.Simple.Types ( Query(..) )
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- | Return constructor name
cName :: (Monad m) => Con -> m Name
cName (NormalC n _) = return n
cName (RecC n _) = return n
cName _ = error "Constructor must be simple"

-- | Return count of constructor fields
cArgs :: (Monad m) => Con -> m Int
cArgs (NormalC _ n) = return $ length n
cArgs (RecC _ n) = return $ length n
cArgs _ = error "Constructor must be simple"

-- | Get field names from record constructor
cFieldNames :: Con -> [Name]
cFieldNames (RecC _ vst) = map (\(a, _, _) -> a) vst
cFieldNames _ = error "Constructor must be a record (product type with field names)"


lookupVNameErr :: String -> Q Name
lookupVNameErr name =
    lookupValueName name >>=
    maybe (error $ "could not find identifier: " ++ name)
          return
