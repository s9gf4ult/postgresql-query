module PGSimple.Config
       (
         PostgresConf(..)
       , establishPGPool
       ) where

import Prelude

import Data.Aeson
import Data.ByteString ( ByteString )
import Data.Pool
import Data.Time

import qualified Database.PostgreSQL.Simple as PG

data PostgresConf = PostgresConf
    { pgConnStr  :: ByteString
      -- ^ The connection string.
    , pgPoolSize :: Int
      -- ^ How many connections should be held on the connection pool.
    , pgPoolTimeout :: NominalDiffTime
      -- ^ Timeout to stay connection active
    , pgPoolStripes :: Int
      -- ^ Stripes in the pool
    } deriving (Ord, Eq, Show)

instance FromJSON PostgresConf where
    parseJSON = withObject "PostgresConf" $ \o -> do
        database <- o .: "database"
        host     <- o .: "host"
        port     <- o .:? "port" .!= 5432
        user     <- o .: "user"
        password <- o .: "password"
        pSize    <- o .: "poolsize"
        pTimeout <- o .:? "pooltimeout" .!= 60
        pStripes <- o .:? "poolstripes" .!= 1
        let ci = PG.ConnectInfo
                   { PG.connectHost     = host
                   , PG.connectPort     = port
                   , PG.connectUser     = user
                   , PG.connectPassword = password
                   , PG.connectDatabase = database
                   }
            cstr = PG.postgreSQLConnectionString ci
        return $ PostgresConf
                 { pgConnStr = cstr
                 , pgPoolSize = pSize
                 , pgPoolTimeout = fromInteger pTimeout
                 , pgPoolStripes = pStripes
                 }

establishPGPool :: PostgresConf -> IO (Pool PG.Connection)
establishPGPool PostgresConf{..} =
    createPool
    (PG.connectPostgreSQL pgConnStr)
    PG.close
    pgPoolStripes
    pgPoolTimeout
    pgPoolSize
