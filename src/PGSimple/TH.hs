module PGSimple.TH
       ( -- * Deriving instances
         deriveFromRow
       , deriveToRow
         -- * Embedding sql files
       , embedSql
       , sqlFile
         -- * Sql string interpolation
       , sqlExp
       , sqlExpEmbed
       , sqlExpFile
       ) where

import Prelude hiding (takeWhile)

import Control.Applicative
import Control.Monad ( when )
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text
import Data.FileEmbed ( embedFile, bsToExp )
import Data.Monoid
import Data.Text ( Text )
import Database.PostgreSQL.Simple.FromRow ( FromRow(..), field )
import Database.PostgreSQL.Simple.ToRow ( ToRow(..) )
import Database.PostgreSQL.Simple.Types ( Query(..) )
import Language.Haskell.Meta.Parse.Careful
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import PGSimple.SqlBuilder
import PGSimple.TH.SqlExp

import qualified Data.Text as T
import qualified Data.Text.Encoding as T


cName :: (Monad m) => Con -> m Name
cName (NormalC n _) = return n
cName (RecC n _) = return n
cName _ = error "Constructor must be simple"

cArgs :: (Monad m) => Con -> m Int
cArgs (NormalC _ n) = return $ length n
cArgs (RecC _ n) = return $ length n
cArgs _ = error "Constructor must be simple"

-- | Derive 'FromRow' instance. i.e. you have type like that
--
-- @
-- data Entity = Entity
--               { eField :: Text
--               , eField2 :: Int
--               , efield3 :: Bool }
-- @
--
-- then 'deriveFromRow' will generate this instance:
-- instance FromRow Entity where
--
-- @
-- instance FromRow Entity where
--     fromRow = Entity
--               \<$> field
--               \<*> field
--               \<*> field
-- @
--
-- Datatype must have just one constructor with arbitrary count of fields
deriveFromRow :: Name -> Q [Dec]
deriveFromRow t = do
    TyConI (DataD _ _ _ [con] _) <- reify t
    cname <- cName con
    cargs <- cArgs con
    [d|instance FromRow $(return $ ConT t) where
           fromRow = $(fieldsQ cname cargs)|]
  where
    fieldsQ cname cargs = do
        fld <- [| field |]
        fmp <- [| (<$>) |]
        fap <- [| (<*>) |]
        return $ UInfixE (ConE cname) fmp (fapChain cargs fld fap)

    fapChain 0 _ _ = error "there must be at least 1 field in constructor"
    fapChain 1 fld _ = fld
    fapChain n fld fap = UInfixE fld fap (fapChain (n-1) fld fap)

lookupVNameErr :: String -> Q Name
lookupVNameErr name =
    lookupValueName name >>=
    maybe (error $ "could not find identifier: " ++ name)
          return


-- | derives 'ToRow' instance for datatype like
--
-- @
-- data Entity = Entity
--               { eField :: Text
--               , eField2 :: Int
--               , efield3 :: Bool }
-- @
--
-- it will derive instance like that:
--
-- @
-- instance ToRow Entity where
--      toRow (Entity e1 e2 e3) =
--          [ toField e1
--          , toField e2
--          , toField e3 ]
-- @
deriveToRow :: Name -> Q [Dec]
deriveToRow t = do
    TyConI (DataD _ _ _ [con] _) <- reify t
    cname <- cName con
    cargs <- cArgs con
    cvars <- sequence
             $ replicate cargs
             $ newName "a"
    [d|instance ToRow $(return $ ConT t) where
           toRow $(return $ ConP cname $ map VarP cvars) = $(toFields cvars)|]
  where
    toFields v = do
        tof <- lookupVNameErr "toField"
        return $ ListE
            $ map
            (\e -> AppE (VarE tof) (VarE e))
            v

-- embed sql file as value
embedSql :: String               -- ^ File path
         -> Q Exp
embedSql path = do
    [e| (Query ( $(embedFile path) )) |]

-- embed sql file by pattern. __sqlFile "dir/file"__ is just the same as
-- __embedSql "sql/dir/file.sql"__
sqlFile :: String                -- ^ sql file pattern
        -> Q Exp
sqlFile s = do
    embedSql $ "sql/" ++ s ++ ".sql"
