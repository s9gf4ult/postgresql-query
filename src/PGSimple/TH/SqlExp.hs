module PGSimple.TH.SqlExp
       ( -- * QQ
         sqlExp
         -- * Types
       , Rope(..)
         -- * Parser
       , ropeParser
       , parseRope
       , squashRope
       , buildQ
         -- * Template haskell
       , sqlQExp
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
import Language.Haskell.TH.Syntax
import PGSimple.SqlBuilder

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


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

-- | Build builder from rope
buildBuilder :: Exp              -- ^ Expression of type 'Query'
             -> Rope
             -> Q Exp
buildBuilder _ (RLit t) = do
    bs <- bsToExp $ T.encodeUtf8 t
    [e| toSqlBuilder $(pure bs) |]
buildBuilder q (RInt t) = do
    when (T.null $ T.strip t) $ fail "empty interpolation string found"
    let ex = either error id $ parseExp $ T.unpack t
    [e| sqlBuilderFromField $(pure q) $(pure ex) |]
buildBuilder _ (RPaste t) = do
    when (T.null $ T.strip t) $ fail "empty paste string found"
    let ex = either error id $ parseExp $ T.unpack t
    [e| toSqlBuilder $(pure ex) |]

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
sqlQExp :: String
        -> Q Exp                 -- ^ Expression of type 'SqlBuilder'
sqlQExp s = do
    let rope = parseRope s
    q <- buildQ rope
    exps <- mapM (buildBuilder q) rope
    [e| ( mconcat $(pure $ ListE exps) ) |]

-- | Embed sql template and perform interpolation
sqlExpEmbed :: String            -- ^ file path
            -> Q Exp             -- ^ Expression of type 'SqlBuilder'
sqlExpEmbed fpath = do
    qAddDependentFile fpath
    s <- runIO $ T.unpack . T.decodeUtf8 <$> B.readFile fpath
    sqlQExp s

-- | Just like 'sqlExpEmbed' but uses pattern instead of file name. __sqlExpFile "dir/template"__ is just the same as __sqlExpEmbed "sql/dir/template.sql"__
sqlExpFile :: String
           -> Q Exp
sqlExpFile ptr = sqlExpEmbed $ "sql/" <> ptr <> ".sql"
