module PGSimple.TH
       ( deriveFromRow
       , deriveToRow
       , embedSql
       , sqlFile
       , sqlExp
         -- * Rope
       , Rope(..)
       , ropeParser
       , squashRope
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

embedSql :: String -> Q Exp
embedSql path = do
    [e| (Query ( $(embedFile path) )) |]

sqlFile :: String -> Q Exp
sqlFile s = do
    embedSql $ "sql/" ++ s ++ ".sql"

sqlExp :: QuasiQuoter
sqlExp = QuasiQuoter
         { quoteExp  = sqlQExp
         , quotePat  = error "sqlInt used in pattern"
         , quoteType = error "sqlInt used in type"
         , quoteDec  = error "sqlInt used in declaration"
         }

data Rope
    = RLit Text                -- ^ Literal SQL string
    | RInt Text               -- ^ String with haskell expression of type __(ToField a) => a__
    | RPaste Text             -- ^ String with haskell expression of type __SqlBuilder__
    deriving (Ord, Eq, Show)

parseRope :: String -> [Rope]
parseRope s = either error id
              $ parseOnly ropeParser
              $ T.pack s

squashRope :: [Rope] -> [Rope]
squashRope ((RLit a):(RLit b):xs) = squashRope ((RLit $ a <> b):xs)
squashRope (x:xs) = x:(squashRope xs)
squashRope [] = []

ropeParser :: Parser [Rope]
ropeParser = fmap squashRope
             $ many1
             $ ropeLit <|> ropeInt <|> ropePaste <|> singleSpecial
  where
    specials = "^#"

    ropeLit = RLit <$> takeWhile1 (`notElem` specials)

    ropeInt = do
        _ <- string "#{"
        ex <- takeWhile (/= '}')
        _ <- char '}'
        return $ RInt ex

    ropePaste = do
        _ <- string "^{"
        ex <- takeWhile (/= '}')
        _ <- char '}'
        return $ RPaste ex

    singleSpecial = (RLit . T.singleton) <$> satisfy (`elem` specials)

buildBuilder :: Exp -> Rope -> Q Exp
buildBuilder _ (RLit t) = do
    bs <- bsToExp $ T.encodeUtf8 t
    [e| sqlBuilderBS $(pure bs) |]
buildBuilder q (RInt t) = do
    when (T.null $ T.strip t) $ fail "empty interpolation string found"
    let ex = either error id $ parseExp $ T.unpack t
    [e| sqlBuilderFromField $(pure q) $(pure ex) |]
buildBuilder _ (RPaste t) = do
    when (T.null $ T.strip t) $ fail "empty paste string found"
    return
        $ either error id
        $ parseExp
        $ T.unpack t

-- | Build 'Query' expression from row
buildQ :: [Rope] -> Q Exp
buildQ r = do
    bs <- bsToExp $ mconcat $ map fromRope r
    [e| Query $(pure bs) |]
  where
    fromRope (RLit t) = T.encodeUtf8 t
    fromRope (RInt _) = "?"
    fromRope (RPaste _) = "(FIXME: interpolate pasting later)"

-- | Build expression of type SqlBuilder from SQL query with interpolation
sqlQExp :: String -> Q Exp
sqlQExp s = do
    let rope = parseRope s
    q <- buildQ rope
    exps <- mapM (buildBuilder q) rope
    [e| ( mconcat $(pure $ ListE exps) ) |]
