module Main where

import BuilderTest
import ParserTest
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "main"
  [ parserTests
  , builderTests ]
