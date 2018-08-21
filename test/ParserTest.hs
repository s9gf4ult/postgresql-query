{-# OPTIONS_GHC -fno-warn-orphans #-}

module ParserTest
       ( parserTests
       ) where

import Data.Attoparsec.Text ( parseOnly )
import Data.Monoid
import Data.Text ( Text )
import Database.PostgreSQL.Query.SqlBuilder
import Database.PostgreSQL.Query.TH.SqlExp
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Assertions
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Modifiers
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.TH

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import qualified Data.Text as T

noSeqSpace :: [Rope] -> [Rope]
noSeqSpace ((RSpaces a):(RSpaces b):xs) = noSeqSpace
                                          $ (RSpaces $ a + b):xs
noSeqSpace (x:xs) = x:(noSeqSpace xs)
noSeqSpace [] = []

newtype RopeList = RopeList [Rope]
  deriving (Ord, Eq, Show)

instance Arbitrary RopeList where
  arbitrary = resize 10 $ (RopeList . noSeqSpace . getNonEmpty) <$> arbitrary

instance Arbitrary FieldOption where
  arbitrary = genericArbitrary
  shrink = genericShrink

wordAlpha :: [Text]
wordAlpha = map T.singleton
            $ ['A'..'Z'] ++ ['a'..'z']
            ++ "!@$%&*()[];|{}<>="

stringAlpha :: [Text]
stringAlpha = wordAlpha ++ ["''", "\\'", " "]

identAlpha :: [Text]
identAlpha = wordAlpha ++ ["\"\"", " "]

intAlpha :: [Text]
intAlpha = wordAlpha ++ [" "]


instance Arbitrary Rope where
    arbitrary =
        oneof [ RLit <$> stringLit
              , RLit <$> idLit
              , RInt <$> arbitrary <*> ropeInt
              , RPaste <$> ropePaste
              , RComment <$> comment
              , RComment <$> bcomment
              , RSpaces <$> spaces
              , RLit <$> wordLit
              ]
      where
        selems = resize 5 . listOf . elements
        selems1 = resize 5 . listOf1 . elements
        stringLit = do
            x <- selems stringAlpha
            return $ "'" <> mconcat x <> "'"
        idLit = do
            x <- selems identAlpha
            return $ "\"" <> mconcat x <> "\""
        ropeInt = do
            x <- selems intAlpha
            return $ "#{" <> mconcat x <> "}"
        ropePaste = do
            x <- selems1 intAlpha
            return $ "^{" <> mconcat x <> "}"
        comment = do
            s <- selems wordAlpha
            return $ "--" <> mconcat s
        bcomment = do
            n <- selems wordAlpha
            return $ "/*" <> mconcat n <> "*/"
        spaces = suchThat arbitrary (>= 1)
        wordLit = fmap mconcat
                  $ selems1 wordAlpha

flattenRope :: [Rope] -> Text
flattenRope = mconcat . map f
  where
    f (RLit t) = t
    f (RComment c) = case T.uncons c of
        (Just ('-', _)) -> c <> "\n"
        _ -> c
    f (RSpaces s) = mconcat $ replicate s " "
    f (RInt FieldDefault t) = "#{" <> quoteBrace t <> "}"
    f (RInt FieldMasked t) = "#?{" <> quoteBrace t <> "}"
    f (RPaste t) = "^{" <> quoteBrace t <> "}"
    quoteBrace = T.replace "}" "\\}"

prop_RopeParser ::  RopeList -> Result
prop_RopeParser (RopeList rope) =
    (Right $ squashRope rope) ==?
    (fmap squashRope $ parseOnly ropeParser $ flattenRope rope)

expectParser :: (Eq a, Show a) => ([Rope] -> a) -> (a, Text) -> Assertion
expectParser predicate (a, raw) = do
  rope <- either fail return $ parseOnly ropeParser raw
  a @=? predicate rope

case_expectLiterals :: Assertion
case_expectLiterals = mapM_ (expectParser takeLits)
    [ ("hello hello", "hello     hello")
    , ("'hello     hello    '",  "'hello     hello    '")
    , (" SELECT ", " SELECT   --     ")
    , ("xxx '  ''  '", "xxx     '  ''  '")
    , ("xxx xxx  xxx", "xxx   xxx --     \n     xxx")
    , (" '    ''\\'' ", "    '    ''\\''     ")
    , ("'''\\'''''\\'' ", "'''\\'''''\\'' ")
    , (" \"    ident    \" ", " \"    ident    \" ")
    , (" one - two -> plus ", " one -    \n  two    -> plus -- \n")
    , ("xxx ", "xxx /* this must eliminate */")
    , ("xxx  xxx", "xxx /* bla /* nested */ \n comment */ xxx")
    ]
  where
    takeLits = mconcat . map takeLit
    takeLit :: Rope -> Text
    takeLit (RLit t) = t
    takeLit (RSpaces _) = " "
    takeLit _ = ""

case_expectComments :: Assertion
case_expectComments = mapM_ (expectParser takeComments)
    [ ("", "hello     hello")
    , ("",  "'hello     hello    '")
    , ("-- line comment", " SELECT   -- line comment")
    , ("/* comment */", "/* comment */ '  /* */ '' --  '")
    ]
  where
    takeComments = mconcat . map takeComment
    takeComment :: Rope -> Text
    takeComment (RComment t) = t
    takeComment _ = ""

case_expectInterpolation :: Assertion
case_expectInterpolation = mapM_ (expectParser takeInterpol)
    [ ("", "  hello    hello ")
    , ("something", " hello #{something} happend")
    , ("something", " hello ^{something} happend")
    , ("something", " hello #?{something} happend")
    , ("", " 'hello #{something} happend' ")
    , ("", " 'hello ^{something} happend' ")
    , ("", " 'hello #?{something} happend' ")
    , ("", " \"hello #{something} happend\" ")
    , ("", " \"hello ^{something} happend\" ")
    , ("", " \"hello #?{something} happend\" ")
    , ("", " -- hello #{something} happend ")
    , ("", " -- hello ^{something} happend' ")
    , ("", " -- hello #?{something} happend' ")
    , ("", " /* hello #{something} happend */ ")
    , ("", " /* hello ^{something} happend */ ")
    , ("", " /* hello #?{something} happend */ ")

    ]
  where
    takeInterpol = mconcat . map go
    go :: Rope -> Text
    go (RInt _ t) = t
    go (RPaste t) = t
    go _ = ""


parserTests :: TestTree
parserTests = $testGroupGenerator
