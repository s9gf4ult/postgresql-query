module Main where

import           Test.DocTest

-- This test suite exists only to add dependencies
main :: IO ()
main = doctest
  [ "-XAutoDeriveTypeable"
  , "-XCPP"
  , "-XConstraintKinds"
  , "-XDataKinds"
  , "-XDeriveDataTypeable"
  , "-XDeriveGeneric"
  , "-XEmptyDataDecls"
  , "-XFlexibleContexts"
  , "-XFlexibleInstances"
  , "-XFunctionalDependencies"
  , "-XGADTs"
  , "-XGeneralizedNewtypeDeriving"
  , "-XLambdaCase"
  , "-XMultiParamTypeClasses"
  , "-XOverloadedStrings"
  , "-XQuasiQuotes"
  , "-XRankNTypes"
  , "-XRecordWildCards"
  , "-XScopedTypeVariables"
  , "-XStandaloneDeriving"
  , "-XTemplateHaskell"
  , "-XTupleSections"
  , "-XTypeFamilies"
  , "-XTypeOperators"
  , "-XUndecidableInstances"
  , "-XViewPatterns"
  , "src" ]
