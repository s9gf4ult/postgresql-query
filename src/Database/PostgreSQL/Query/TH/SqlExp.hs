module Database.PostgreSQL.Query.TH.SqlExp
       ( -- * QQ
         sqlExp
         -- * Types
       , Rope(..)
         -- * Parser
       , ropeParser
       , parseRope
       , squashRope
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
import Database.PostgreSQL.Query.SqlBuilder
#if MIN_VERSION_haskell_src_meta(0,8,0)
import Language.Haskell.Meta.Parse
#else
import Language.Haskell.Meta.Parse.Careful
#endif
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

{- $setup
>>> import Database.PostgreSQL.Simple
>>> import Database.PostgreSQL.Query.SqlBuilder
>>> import Data.Text ( Text )
>>> import qualified Data.List as L
>>> c <- connect defaultConnectInfo
-}

{- | Maybe the main feature of all library. Quasiquoter which builds
'SqlBuilder' from string query. Removes line comments and block
comments (even nested) and sequences of spaces. Correctly works
handles string literals and quoted identifiers. Here is examples of usage

>>> let name = "name"
>>> let val = "some 'value'"
>>> runSqlBuilder c [sqlExp|SELECT * FROM tbl WHERE ^{mkIdent name} = #{val}|]
"SELECT * FROM tbl WHERE \"name\" = 'some ''value'''"

And more comples example:

>>> let name = Just "name"
>>> let size = Just 10
>>> let active = Nothing :: Maybe Bool
>>> let condlist = catMaybes [ fmap (\a -> [sqlExp|name = #{a}|]) name, fmap (\a -> [sqlExp|size = #{a}|]) size, fmap (\a -> [sqlExp|active = #{a}|]) active]
>>> let cond = if L.null condlist then mempty else [sqlExp| WHERE ^{mconcat $ L.intersperse " AND " $ condlist} |]
>>> runSqlBuilder c [sqlExp|SELECT *   FROM tbl ^{cond} -- line comment|]
"SELECT * FROM tbl  WHERE name = 'name' AND size = 10  "

-}

sqlExp :: QuasiQuoter
sqlExp = QuasiQuoter
         { quoteExp  = sqlQExp
         , quotePat  = error "sqlInt used in pattern"
         , quoteType = error "sqlInt used in type"
         , quoteDec  = error "sqlInt used in declaration"
         }

-- | Internal type. Result of parsing sql string
data Rope
    = RLit Text             -- ^ Part of raw sql
    | RComment Text         -- ^ Sql comment
    | RSpaces Int           -- ^ Sequence of spaces
    | RInt FieldOption Text -- ^ String with haskell expression inside __#{..}__
                            -- or __#?{..}__
    | RPaste Text           -- ^ String with haskell expression inside __^{..}__
    deriving (Ord, Eq, Show)

parseRope :: String -> [Rope]
parseRope s = either error id
              $ parseOnly ropeParser
              $ T.pack s

ropeParser :: Parser [Rope]
ropeParser = many1 $ choice
             [ quoted
             , iquoted
             , RInt FieldMasked <$> someNested "#?{"
             , RInt FieldDefault <$> someNested "#{"
             , RPaste <$> someNested "^{"
             , comment
             , bcomment
             , spaces
             , (RLit . T.singleton) <$> anyChar
             ]
  where
    eofErf e p =
        choice
        [ endOfInput
          *> (error $ "Unexpected end of input: " <> e)
        , p
        ]

    unquoteBraces = T.replace "\\}" "}"

    -- Prefix must be string like '#{' or something
    someNested :: Text -> Parser Text
    someNested prefix = do
        _ <- string prefix
        e <- many1 $ choice
             [ string "\\}"
             , T.singleton <$> notChar '}'
             ]
        eofErf ("block " <> T.unpack prefix <> " not finished") $ do
          _ <- char '}'
          return $ unquoteBraces $ mconcat e

    comment = do
        b <- string "--"
        c <- takeWhile (`notElem` ['\r', '\n'])
        endOfLine <|> endOfInput
        return $ RComment $ b <> c
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
buildBuilder :: Rope
             -> Maybe (Q Exp)
buildBuilder (RLit t) = Just $ do
    bs <- bsToExp $ T.encodeUtf8 t
    [e| sqlBuilderFromByteString $(pure bs) |]
buildBuilder (RInt fo t) = Just $ do
    when (T.null $ T.strip t) $ fail "empty interpolation string found"
    let ex = either error id $ parseExp $ T.unpack t
    [e| sqlBuilderFromField $(lift fo) $(pure ex) |]
buildBuilder (RPaste t) = Just $ do
    when (T.null $ T.strip t) $ fail "empty paste string found"
    let ex = either error id $ parseExp $ T.unpack t
    [e| toSqlBuilder $(pure ex) |]
buildBuilder _ = Nothing

-- | Removes sequential occurencies of 'RLit' constructors. Also
-- removes commentaries and squash sequences of spaces to single space
-- symbol
squashRope :: [Rope] -> [Rope]
squashRope = go . catMaybes . map cleanRope
  where
    cleanRope (RComment _) = Nothing
    cleanRope (RSpaces _) = Just $ RLit " "
    cleanRope x = Just x

    go ((RLit a):(RLit b):xs) = go ((RLit $ a <> b):xs)
    go (x:xs) = x:(go xs)
    go [] = []

-- | Build expression of type 'SqlBuilder' from SQL query with interpolation
sqlQExp :: String
        -> Q Exp                 -- ^ Expression of type 'SqlBuilder'
sqlQExp s = do
    let rope = squashRope $ parseRope s
    exps <- sequence
            $ catMaybes
            $ map buildBuilder rope
    [e| ( mconcat $(pure $ ListE exps) ) |]

{- | Embed sql template and perform interpolation

@
let name = "name"
    foo = "bar"
    query = $(sqlExpEmbed "sql/foo/bar.sql") -- using 'foo' and 'bar' inside
@
-}

sqlExpEmbed :: String            -- ^ file path
            -> Q Exp             -- ^ Expression of type 'SqlBuilder'
sqlExpEmbed fpath = do
    qAddDependentFile fpath
    s <- runIO $ T.unpack . T.decodeUtf8 <$> B.readFile fpath
    sqlQExp s

{- | Just like 'sqlExpEmbed' but uses pattern instead of file
name. So, code

@
let query = $(sqlExpFile "foo/bar")
@

is just the same as

@
let query = $(sqlExpEmbed "sql/foo/bar.sql")
@

This function inspired by Yesod's 'widgetFile'
-}

sqlExpFile :: String
           -> Q Exp
sqlExpFile ptr = sqlExpEmbed $ "sql/" <> ptr <> ".sql"
