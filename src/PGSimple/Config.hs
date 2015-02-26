module PGSimple.Config
       (
         PostgresConf(..)
       , PGPool(..)
       , createPGPool
       , pingPGPool
       , withPGPool
       , withPGPoolPrim
       ) where

import Prelude

import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Aeson
import Data.ByteString ( ByteString )
import Data.Pool
import Data.Time

import qualified Database.PostgreSQL.Simple as PG

-- | Connection pool. Must be created from settings using 'createPGPool'
newtype PGPool = PGPool (Pool PG.Connection)

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
        database <- o .:  "database"
        host     <- o .:? "host"        .!= "127.0.0.1"
        port     <- o .:? "port"        .!= 5432
        user     <- o .:  "user"
        password <- o .:  "password"
        pSize    <- o .:? "poolsize"    .!= 10
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

createPGPool :: PostgresConf -> IO PGPool
createPGPool PostgresConf{..} =
    fmap PGPool
    $ createPool
    (PG.connectPostgreSQL pgConnStr)
    PG.close
    pgPoolStripes
    pgPoolTimeout
    pgPoolSize


{- | Combinator for simple implementation of 'withPGConnection' method.
typical usage is:

@
instance HasPostgres (HandlerT App IO) where
    withPGConnection = withPGPool appPGPool
@
-}
withPGPool :: (MonadReader site m, MonadBaseControl IO m)
           => (site -> PGPool)
           -> (PG.Connection -> m a)
           -> m a
withPGPool extract action = do
    (PGPool pool) <- asks extract
    withResource pool action

{- | Another combinator to implement 'withPGConnection'

@
instance HasPostgres (OurMonadT IO) where
    withPGConnection = withPGPoolPrim $ getPGPool \<$\> getSomeThing
@
-}
withPGPoolPrim :: (MonadBaseControl IO m)
               => m PGPool
               -> (PG.Connection -> m a)
               -> m a
withPGPoolPrim pget action = do
    (PGPool pool) <- pget
    withResource pool action

-- | Force to create at least one connection in pool. Usefull to check
-- connection settings at program start time
pingPGPool :: PGPool -> IO ()
pingPGPool (PGPool pool) = withResource pool $ const (return ())
