{-# OPTIONS -fno-warn-orphans #-}

module Main where

import Data.Attoparsec.Text ( parseOnly )
import Data.Monoid
import Data.Text ( Text )
import PGSimple.TH.SqlExp
import Test.QuickCheck.Assertions
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Modifiers
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.TH

import qualified Data.Text as T

instance Arbitrary Rope where
    arbitrary = oneof [ fmap RLit noSpecialString
                      , fmap RInt nobraceString
                      , fmap RPaste nobraceString
                      ]
      where
        noSpecialString = suchThat arbitrary (\x -> and [ not $ T.isInfixOf "#{" x
                                                       , not $ T.isInfixOf "^{" x
                                                       , T.length x > 0])
        nobraceString = suchThat arbitrary (\x -> and [ not $ T.isInfixOf "}" x
                                                      ])


flattenRope :: [Rope] -> Text
flattenRope = mconcat . map f
  where
    f (RLit t) = t
    f (RInt t) = "#{" <> t <> "}"
    f (RPaste t) = "^{" <> t <> "}"

prop_RopeParser ::  NonEmptyList Rope -> Result
prop_RopeParser (NonEmpty rope) =
    (Right $ squashRope rope) ==? (fmap squashRope $ parseOnly ropeParser $ flattenRope rope)

case_cleanLit =
    mapM_ (\(a, b) -> a @=? (cleanLit b))
    [ ("hello hello", "hello     hello")
    , ("'hello     hello    '",  "'hello     hello    '")
    , (" SELECT ", " SELECT   --     ")
    , ("xxx '  ''  '", "xxx     '  ''  '")
    , ("xxx xxx  xxx", "xxx   xxx --     \n     xxx")
    , (" '    ''\\'' ", "    '    ''\\''     ")
    , ("'''\\'''''\\'' ", "'''\\'''''\\'' ")
    , (" \"    ident    \" ", " \"    ident    \" ")
    , (" one - two -> plus ", " one -    \n  two    -> plus -- \n")
    ]

mainGroup = $testGroupGenerator

main :: IO ()
main = do
    defaultMain mainGroup
