module BuilderTest
       ( builderTests
       ) where

import Data.Text (Text)
import Database.PostgreSQL.Query
import Database.PostgreSQL.Simple
import Test.Tasty
import Test.Tasty.HUnit

caseLogDefault :: IO Connection -> TestTree
caseLogDefault iocon = testCase "expect parameter pasted" $ do
  con <- iocon
  let t = "TEXT" :: Text
  (q, l) <- runSqlBuilder con defaultLogMasker [sqlExp|INSERT #{t}|]
  q @?= "INSERT 'TEXT'"
  l @?= "INSERT 'TEXT'"

caseLogMask :: IO Connection -> TestTree
caseLogMask iocon = testCase "exptect parameter masked" $ do
  con <- iocon
  let t = "TEXT" :: Text
  (q, l) <- runSqlBuilder con defaultLogMasker [sqlExp|INSERT #?{t}|]
  q @?= "INSERT 'TEXT'"
  l @?= "INSERT '<MASKED BY POSTGRESQL-QUERY>'"


builderTests :: TestTree
builderTests =
  withResource (connectPostgreSQL "") close $ \con -> testGroup "BuilderTest"
  [ caseLogMask con
  , caseLogDefault con
  ]
