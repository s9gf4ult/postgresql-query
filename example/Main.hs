module Main where

import Data.Text (Text)
import Data.Time
import Database.PostgreSQL.Query
import Text.Inflections

-- | Example enum type to check out how 'derivePgEnum' works
data Species
  = Dog
  | Cat
  | Snake

derivePgEnum toUnderscore ''Species

-- | Example structure to check out how 'deriveFromRow' works
data AnimalInfo = AnimalInfo
  { _aiName    :: Text
  , _aiSpecies :: Species
  , _aiBirtDay :: UTCTime
  }

deriveFromRow ''AnimalInfo
deriveToRow ''AnimalInfo

main :: IO ()
main = do
  return ()
