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
import Data.Maybe
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
    = RLit Text            -- ^ Part of raw sql
    | RComment Text        -- ^ Sql comment
    | RSpaces Int          -- ^ Sequence of spaces
    | RInt Text            -- ^ String with haskell expression inside __#{..}__
    | RPaste Text          -- ^ String with haskell expression inside __^{..}__
    deriving (Ord, Eq, Show)

parseRope :: String -> [Rope]
parseRope s = either error id
              $ parseOnly ropeParser
              $ T.pack s

ropeParser :: Parser [Rope]
ropeParser = many1 $ choice
             [ quoted
             , iquoted
             , ropeint
             , ropepaste
             , comment
             , bcomment
             , spaces
             , word
             ]
  where
    eofErf e p =
        choice
        [ endOfInput
          *> (error $ "Unexpected end of input: " <> e)
        , p
        ]

    unquoteBraces = T.replace "\\}" "}"

    ropeint = do
        _ <- string "#{"
        e <- many1 $ choice
             [ string "\\}"
             , T.singleton <$> notChar '}'
             ]
        _ <- char '}'
        return $ RInt $ unquoteBraces $ mconcat e

    ropepaste = do
        _ <- string "^{"
        e <- many1 $ choice
             [ string "\\}"
             , T.singleton <$> notChar '}'
             ]
        _ <- char '}'
        return $ RPaste $ unquoteBraces $ mconcat e

    comment = do
        b <- string "--"
        c <- takeWhile (`notElem` ['\r', '\n'])
        endOfLine <|> endOfInput
        return $ RComment $ b <> c
    word = RLit <$> takeWhile1 isWord
    isWord ch = not $ isSpace ch || elem ch ['#', '^', '/', '-', '\'', '"']
    spaces = (RSpaces . T.length) <$> takeWhile1 isSpace

    bcomment :: Parser Rope
    bcomment = RComment <$> go
      where
        go = do
            b <- string "/*"
            c <- many' $ choice
                 [ go
                 , justStar
                 , T.singleton <$> notChar '*'
                 ]
            eofErf "block comment not finished, maybe typo" $ do
                e <- string "*/"
                return $ b <> mconcat c <> e
        justStar = do
            _ <- char '*'
            peekChar >>= \case
                (Just '/') -> fail "no way"
                _ -> return "*"

    quoted = do
        _ <- char '\''
        ret <- many' $ choice
               [ string "''"
               , string "\\'"
               , T.singleton <$> notChar '\''
               ]
        eofErf "string literal not finished" $ do
            _ <- char '\''
            return $ RLit $ "'" <> mconcat ret <> "'"

    iquoted = do
        _ <- char '"'
        ret <- many' $ choice
               [ string "\"\""
               , T.singleton <$> notChar '"'
               ]
        eofErf "quoted identifier not finished" $ do
            _ <- char '"'
            return $ RLit $ "\"" <> mconcat ret <> "\""


-- | Build builder from rope
buildBuilder :: Exp              -- ^ Expression of type 'Query'
             -> Rope
             -> Maybe (Q Exp)
buildBuilder _ (RLit t) = Just $ do
    bs <- bsToExp $ T.encodeUtf8 t
    [e| toSqlBuilder $(pure bs) |]
buildBuilder q (RInt t) = Just $ do
    when (T.null $ T.strip t) $ fail "empty interpolation string found"
    let ex = either error id $ parseExp $ T.unpack t
    [e| sqlBuilderFromField $(pure q) $(pure ex) |]
buildBuilder _ (RPaste t) = Just $ do
    when (T.null $ T.strip t) $ fail "empty paste string found"
    let ex = either error id $ parseExp $ T.unpack t
    [e| toSqlBuilder $(pure ex) |]
buildBuilder _ _ = Nothing

-- | Build 'Query' expression from row
buildQ :: [Rope] -> Q Exp
buildQ r = do
    bs <- bsToExp $ mconcat $ map fromRope r
    [e| Query $(pure bs) |]
  where
    fromRope (RLit t) = T.encodeUtf8 t
    fromRope (RInt t) = "#{" <> (T.encodeUtf8 t) <> "}"
    fromRope (RPaste p) = "^{" <> (T.encodeUtf8 p) <> "}"
    fromRope (RComment c) = T.encodeUtf8 c
    fromRope (RSpaces _) = " "

squashRope :: [Rope] -> [Rope]
squashRope = go . catMaybes . map cleanRope
  where
    cleanRope (RComment _) = Nothing
    cleanRope (RSpaces _) = Just $ RLit " "
    cleanRope x = Just x

    go ((RLit a):(RLit b):xs) = go ((RLit $ a <> b):xs)
    go (x:xs) = x:(go xs)
    go [] = []

-- | Build expression of type SqlBuilder from SQL query with interpolation
sqlQExp :: String
        -> Q Exp                 -- ^ Expression of type 'SqlBuilder'
sqlQExp s = do
    let rope = squashRope
               $ parseRope s
    q <- buildQ rope
    exps <- sequence
            $ catMaybes
            $ map (buildBuilder q) rope
    [e| ( mconcat $(pure $ ListE exps) ) |]

-- | Embed sql template and perform interpolation
sqlExpEmbed :: String            -- ^ file path
            -> Q Exp             -- ^ Expression of type 'SqlBuilder'
sqlExpEmbed fpath = do
    qAddDependentFile fpath
    s <- runIO $ T.unpack . T.decodeUtf8 <$> B.readFile fpath
    sqlQExp s

-- | Just like 'sqlExpEmbed' but uses pattern instead of file name. __sqlExpFile
-- "dir/template"__ is just the same as __sqlExpEmbed "sql/dir/template.sql"__
sqlExpFile :: String
           -> Q Exp
sqlExpFile ptr = sqlExpEmbed $ "sql/" <> ptr <> ".sql"
