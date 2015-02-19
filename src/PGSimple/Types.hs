module PGSimple.Types
       ( -- * Usable fields
         InetText(..)
         -- * Connection pooling
       , HasPostgres(..)
         -- * Query execution monad
       , PgMonadT(..)
       , runPgMonadT
       , launchPG
         -- * Entity model
       , Entity(..)
       , Ent
       ) where


import Prelude

import Blaze.ByteString.Builder ( toByteString )
import Control.Applicative ( Alternative, Applicative )
import Control.Monad ( MonadPlus )
import Control.Monad.Base ( MonadBase(..) )
import Control.Monad.Catch
    ( MonadThrow, MonadMask(mask), MonadCatch, onException )
import Control.Monad.Cont.Class ( MonadCont )
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Reader
    ( MonadFix, MonadTrans, ReaderT(..),
      MonadReader(..), withReaderT )
import Control.Monad.State.Class ( MonadState )
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe
import Control.Monad.Writer.Class ( MonadWriter )
import Data.Int ( Int64 )
import Data.Monoid ( Monoid )
import Data.Pool ( Pool, withResource )
import Data.Proxy ( Proxy )
import Data.String ( IsString )
import Data.Text ( Text )
import Data.Typeable ( Typeable )
import Database.PostgreSQL.Simple
    ( ToRow, Connection, FromRow, rollback,
      commit, begin, execute_, returning,
      query_, query, executeMany, execute )
import Database.PostgreSQL.Simple.FromField
    ( ResultError(..), FromField(..), typename, returnError )
import Database.PostgreSQL.Simple.ToField
    ( Action, ToField )
import Database.PostgreSQL.Simple.Transaction
    ( TransactionMode, defaultTransactionMode, beginMode )
import Database.PostgreSQL.Simple.Types
    ( Query(..) )
import PGSimple.SqlBuilder

import qualified Data.Text as T
import qualified Data.Text.Encoding as T



-- | type to put and get from db 'inet' and 'cidr' typed postgresql
-- fields. This should be in postgresql-simple in fact.
newtype InetText =
    InetText
    { unInetText :: T.Text
    } deriving ( IsString, Eq, Ord, Read, Show
               , Typeable, Monoid, ToField )

instance FromField InetText where
    fromField fld Nothing = returnError ConversionFailed
                            fld "can not convert Null to InetText"
    fromField fld (Just bs) = do
        n <- typename fld
        case n of
            "inet" -> result
            "cidr" -> result
            _ -> returnError
                 ConversionFailed fld
                 "could not convert to InetText"
      where
        result = return $ InetText
                 $ T.decodeUtf8 bs


class (MonadBase IO m) => HasPostgres m where
    withPGConnection :: (Connection -> m a) -> m a

instance (HasPostgres m) => HasPostgres (ReaderT r m) where
    withPGConnection action = do
        r <- ask
        lift $ withPGConnection $ \con ->
            runReaderT (action con) r

instance (HasPostgres m) => HasPostgres (EitherT e m) where
    withPGConnection action = do
        EitherT $ withPGConnection $ \con -> do
            runEitherT $ action con

instance (HasPostgres m) => HasPostgres (MaybeT m) where
    withPGConnection action = do
        MaybeT $ withPGConnection $ \con -> do
            runMaybeT $ action con

newtype PgMonadT m a =
    PgMonadT
    { unPgMonadT :: ReaderT Connection m a
    } deriving ( Functor, Applicative, Monad , MonadWriter w
               , MonadState s, MonadError e, MonadTrans
               , Alternative, MonadFix, MonadPlus, MonadIO
               , MonadCont , MonadThrow, MonadCatch, MonadMask
               , MonadBase b )

instance (MonadBaseControl b m) => MonadBaseControl b (PgMonadT m) where
    type StM (PgMonadT m) a = StM (ReaderT Connection m) a
    liftBaseWith action = PgMonadT $ do
        liftBaseWith $ \runInBase -> action (runInBase . unPgMonadT)
    restoreM st = PgMonadT $ restoreM st


instance MonadTransControl PgMonadT
         --  FIXME: implement


instance (MonadReader r m) => MonadReader r (PgMonadT m) where
    ask = lift ask
    local md ac = do
        con <- PgMonadT ask
        lift $ do
            r <- ask
            local md $ runPgMonadT con ac
    reader = lift . reader


instance (MonadBase IO m) => HasPostgres (PgMonadT m) where
    withPGConnection action = do
        con <- PgMonadT ask
        action con



runPgMonadT :: Connection -> PgMonadT m a -> m a
runPgMonadT con (PgMonadT action) = runReaderT action con

-- | Use 'HasPostgres' instnace to run 'ReaderT Connection m' monad.
-- Usage example:
--
-- @
-- handler :: Handler [Int]
-- handler = launchPG $ do
--     mExecute "INSERT INTO tbl(val) values (?)" [10]
--     a <- mQuery_ "SELECT val FROM tbl"
--     return a
-- @
launchPG :: (HasPostgres m)
         => PgMonadT m a
         -> m a
launchPG act = withPGConnection $ \con -> do
    runPgMonadT con act


-- | Auxiliary typeclass for data types which can map to rows of some
-- table. This typeclass is used inside functions like 'pgSelectEntities' to
-- generate queries.
class Entity a where
    -- | Id type for this entity
    data EntityId a :: *
    -- | Table name of this entity
    tableName :: Proxy a -> Text
    -- | Field names without 'id' and 'created'. The order of field names must match
    -- with order of fields in 'ToRow' and 'FromRow' instances of this type.
    fieldNames :: Proxy a -> [Text]

deriving instance Typeable EntityId

type Ent a = (EntityId a, a)
