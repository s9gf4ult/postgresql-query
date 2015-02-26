{-# OPTIONS -fno-warn-orphans #-}

module Main where

import Control.Applicative
import Data.Attoparsec.Text ( parseOnly )
import Data.Monoid
import Data.Text ( Text )
import Database.PostgreSQL.Query.TH.SqlExp
import Test.QuickCheck.Assertions
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Property
    ( Result )
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH

import qualified Data.Text as T

noSeqSpace :: [Rope] -> [Rope]
noSeqSpace ((RSpaces a):(RSpaces b):xs) = noSeqSpace
                                          $ (RSpaces $ a + b):xs
noSeqSpace (x:xs) = x:(noSeqSpace xs)
noSeqSpace [] = []

newtype RopeList
    = RopeList [Rope]
    deriving (Ord, Eq, Show)

instance Arbitrary RopeList where
    arbitrary =
        resize 10 $ (RopeList . noSeqSpace . getNonEmpty) <$> arbitrary

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
              , RInt <$> ropeInt
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
    f (RInt t) = "#{" <> quoteBrace t <> "}"
    f (RPaste t) = "^{" <> quoteBrace t <> "}"
    quoteBrace = T.replace "}" "\\}"

prop_RopeParser ::  RopeList -> Result
prop_RopeParser (RopeList rope) =
    (Right $ squashRope rope) ==?
    (fmap squashRope $ parseOnly ropeParser $ flattenRope rope)

-- case_cleanLit :: Assertion
-- case_cleanLit =
--     mapM_ (\(a, b) -> a @=? (cleanLit b))
--     [ ("hello hello", "hello     hello")
--     , ("'hello     hello    '",  "'hello     hello    '")
--     , (" SELECT ", " SELECT   --     ")
--     , ("xxx '  ''  '", "xxx     '  ''  '")
--     , ("xxx xxx  xxx", "xxx   xxx --     \n     xxx")
--     , (" '    ''\\'' ", "    '    ''\\''     ")
--     , ("'''\\'''''\\'' ", "'''\\'''''\\'' ")
--     , (" \"    ident    \" ", " \"    ident    \" ")
--     , (" one - two -> plus ", " one -    \n  two    -> plus -- \n")
--     , ("xxx ", "xxx /* thus must eliminate */")
--     , ("xxx  xxx", "xxx /* bla /* nested */ \n comment */ xxx")
--     ]

mainGroup :: TestTree
mainGroup = $testGroupGenerator

main :: IO ()
main = do
    defaultMain mainGroup
