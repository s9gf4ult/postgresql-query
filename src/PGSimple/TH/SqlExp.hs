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
import Data.Char ( isSpace )
import Data.FileEmbed ( bsToExp )
import Data.Monoid
import Data.Text ( Text )
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


cleanLiterals :: [Rope] -> [Rope]
cleanLiterals [] = []
cleanLiterals ((RLit t):xs) = (RLit $ cleanLit t):(cleanLiterals xs)
cleanLiterals (x:xs) = x:(cleanLiterals xs)

-- | Remove sequential spaces and line comments
cleanLit :: Text -> Text
cleanLit t = either error id
             $ parseOnly go t
  where
    go = fmap mconcat
         $ many' tok
    tok = quoted <|> dquoted <|> comment <|> word <|> spaces <|> minus
    minus = do
        _ <- char '-'
        peekChar >>= \case
            (Just '-') -> fail ""
            _ -> return "-"
    comment = do
        _ <- string "--"
        skipWhile (`notElem` ['\r', '\n'])
        endOfLine
        return ""
    word = takeWhile1 isWord
    isWord ch = not $ isSpace ch || elem ch ['\'', '"', '-']
    spaces = takeWhile1 isSpace *> return " "
    quoted = do
        _ <- char '\''
        b <- qbody '\''
        return $ "'" <> b <> "'"
    dquoted = do
        _ <- char '"'
        b <- qbody '"'
        return $ "\"" <> b <> "\""
    peekCharErr e = peekChar >>= \case -- peek char or fail all parser
        Nothing -> error e
        Just r -> return r
    anyCharErr e = peekCharErr e <* anyChar -- get char or fail parser

    qbody :: Char -> Parser Text
    qbody qch = qbodygo ""
      where
        qbodygo acc = do
            x <- takeWhile (`notElem` ['\\', qch])
            ch1 <- anyCharErr "End of quotation without ending quote symbol"
            case ch1 of
                '\\' -> do           -- eat any symbol after backslash
                    ch2 <- anyCharErr "Backshash can not be at the end of literal string"
                    qbodygo $ acc <> x <> (T.pack [ch1, ch2])
                ((== qch) -> True) -> do           -- eat double single quote or stop
                    ch2 <- peekChar
                    case ch2 of
                        ((== (Just qch)) -> True) -> do
                            _ <- anyChar -- consume observed '\'' char
                            qbodygo $ acc <> x <> (T.pack [qch, qch])
                        _ -> return $ acc <> x
                _ -> error "wrong symbol found, this is a bug!"



ropeParser :: Parser [Rope]
ropeParser = many1
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
    let rope = cleanLiterals
               $ squashRope
               $ parseRope s
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
