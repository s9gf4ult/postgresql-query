module Database.PostgreSQL.Query.TH.Common
  ( cName
  , cArgs
  , cFieldNames
  , lookupVNameErr
  , dataConstructors
  ) where

import Prelude

import Language.Haskell.TH

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


dataConstructors :: Info -> [Con]
dataConstructors = \case
  TyConI d ->
#if MIN_VERSION_template_haskell(2,11,0)
    let DataD _ _ _ _ cs _ = d
#else
    let DataD _ _ _ cs _ = d
#endif
    in cs
  x -> error $ "Expected type constructor, " ++ show x ++ " got"
