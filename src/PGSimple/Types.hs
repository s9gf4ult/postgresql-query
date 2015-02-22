module PGSimple.Types
       ( -- * Usable fields
         InetText(..)
         -- * Connection pooling
       , HasPostgres(..)
       , TransactionSafe(..)
         -- * Query execution monad
       , PgMonadT(..)
       , runPgMonadT
       , launchPG
         -- * Entity model
       , Entity(..)
       , Ent
       ) where


import Prelude


import Control.Applicative
import Control.Monad
import Control.Monad.Base ( MonadBase(..) )
import Control.Monad.Catch
    ( MonadThrow, MonadMask, MonadCatch )
import Control.Monad.Cont.Class ( MonadCont )
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Fix ( MonadFix(..) )
import Control.Monad.Logger
import Control.Monad.Reader
    ( MonadReader(..), ReaderT(..) )
import Control.Monad.State.Class ( MonadState )
import Control.Monad.Trans
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Control
import Control.Monad.Trans.Either
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Writer.Class ( MonadWriter )
import Data.Monoid ( Monoid )
import Data.Proxy ( Proxy )
import Data.String ( IsString )
import Data.Text ( Text )
import Data.Typeable ( Typeable )
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
    ( FromField(..), typename, returnError )
import Database.PostgreSQL.Simple.ToField
    ( ToField )

import qualified Control.Monad.Trans.State.Lazy as STL
import qualified Control.Monad.Trans.State.Strict as STS
import qualified Control.Monad.Trans.Writer.Lazy as WL
import qualified Control.Monad.Trans.Writer.Strict as WS
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

instance (HasPostgres m) => HasPostgres (EitherT e m) where
    withPGConnection action = do
        EitherT $ withPGConnection $ \con -> do
            runEitherT $ action con

#if MIN_VERSION_transformers(0, 4, 0)
instance (HasPostgres m) => HasPostgres (ExceptT e m) where
    withPGConnection action = do
        ExceptT $ withPGConnection $ \con -> do
            runExceptT $ action con
#endif

instance (HasPostgres m) => HasPostgres (IdentityT m) where
    withPGConnection action = do
        IdentityT $ withPGConnection $ \con -> do
            runIdentityT $ action con

instance (HasPostgres m) => HasPostgres (MaybeT m) where
    withPGConnection action = do
        MaybeT $ withPGConnection $ \con -> do
            runMaybeT $ action con

instance (HasPostgres m) => HasPostgres (ReaderT r m) where
    withPGConnection action = do
        ReaderT $ \r -> withPGConnection $ \con ->
            runReaderT (action con) r

instance (HasPostgres m) => HasPostgres (STL.StateT s m) where
    withPGConnection action = do
        STL.StateT $ \s -> withPGConnection $ \con ->
            STL.runStateT (action con) s

instance (HasPostgres m) => HasPostgres (STS.StateT s m) where
    withPGConnection action = do
        STS.StateT $ \s -> withPGConnection $ \con ->
            STS.runStateT (action con) s

instance (HasPostgres m) => HasPostgres (ContT r m) where
    withPGConnection action = do
        ContT $ \r -> withPGConnection $ \con ->
            runContT (action con) r

instance (HasPostgres m, Monoid w) => HasPostgres (WL.WriterT w m) where
    withPGConnection action = do
        WL.WriterT $ withPGConnection $ \con ->
            WL.runWriterT (action con)

instance (HasPostgres m, Monoid w) => HasPostgres (WS.WriterT w m) where
    withPGConnection action = do
        WS.WriterT $ withPGConnection $ \con ->
            WS.runWriterT (action con)

-- | Empty typeclass signing monad in which transaction is
-- safe. i.e. `PgMonadT` have this instance, but some other monad giving
-- connection from e.g. connection pool is not.
class TransactionSafe (m :: * -> *)

instance (TransactionSafe m) => TransactionSafe (EitherT e m)
instance (TransactionSafe m) => TransactionSafe (ExceptT e m)
instance (TransactionSafe m) => TransactionSafe (IdentityT m)
instance (TransactionSafe m) => TransactionSafe (MaybeT m)
instance (TransactionSafe m) => TransactionSafe (ReaderT r m)
instance (TransactionSafe m) => TransactionSafe (STL.StateT s m)
instance (TransactionSafe m) => TransactionSafe (STS.StateT s m)
instance (TransactionSafe m) => TransactionSafe (ContT r m)
instance (TransactionSafe m, Monoid w) => TransactionSafe (WL.WriterT w m)
instance (TransactionSafe m, Monoid w) => TransactionSafe (WS.WriterT w m)


newtype PgMonadT m a =
    PgMonadT
    { unPgMonadT :: ReaderT Connection m a
    } deriving ( Functor, Applicative, Monad , MonadWriter w
               , MonadState s, MonadError e, MonadTrans
               , Alternative, MonadFix, MonadPlus, MonadIO
               , MonadCont, MonadThrow, MonadCatch, MonadMask
               , MonadBase b, MonadLogger )

#if MIN_VERSION_monad_control(1,0,0)
instance (MonadBaseControl b m) => MonadBaseControl b (PgMonadT m) where
    type StM (PgMonadT m) a = StM (ReaderT Connection m) a
    liftBaseWith action = PgMonadT $ do
        liftBaseWith $ \runInBase -> action (runInBase . unPgMonadT)
    restoreM st = PgMonadT $ restoreM st
    {-# INLINE liftBaseWith #-}
    {-# INLINE restoreM #-}

instance MonadTransControl PgMonadT where
    type StT PgMonadT a = StT (ReaderT Connection) a
    liftWith action = PgMonadT $ do
        liftWith $ \runTrans -> action (runTrans . unPgMonadT)
    restoreT st = PgMonadT $ restoreT st
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}
#else
instance (MonadBaseControl b m) => MonadBaseControl b (PgMonadT m) where
    newtype StM (PgMonadT m) a
        = PgMTM
        { unPgMTM :: StM (ReaderT Connection m) a
        }
    liftBaseWith action = PgMonadT $ do
        liftBaseWith $ \runInBase -> do
            action ((PgMTM `liftM`) . runInBase . unPgMonadT)
    restoreM (PgMTM st) = PgMonadT $ restoreM st

instance MonadTransControl PgMonadT where
    newtype StT PgMonadT a
        = PgMTT
          { unPgMTT :: StT (ReaderT Connection) a
          }
    liftWith action = PgMonadT $ do
        liftWith $ \runTrans -> do -- ReaderT Connection n a -> n (StT (ReaderT Connection n) a)
            action ((PgMTT `liftM`) . runTrans . unPgMonadT)

    restoreT st = PgMonadT $ restoreT $ unPgMTT `liftM` st
#endif

instance (MonadReader r m) => MonadReader r (PgMonadT m) where
    ask = lift ask
    local md ac = do
        con <- PgMonadT ask
        lift $ do
            local md $ runPgMonadT con ac
    reader = lift . reader


instance (MonadBase IO m) => HasPostgres (PgMonadT m) where
    withPGConnection action = do
        con <- PgMonadT ask
        action con

instance TransactionSafe (PgMonadT m)


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
