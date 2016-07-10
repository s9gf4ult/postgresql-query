module Database.PostgreSQL.Query.Types
       ( -- * Query execution
         MonadPostgresPool(..)
       -- , TransactionSafe
       -- , PgMonadT(..)
       -- , runPgMonadT
       -- , launchPG
       , MonadTransaction(..)
       , MonadPostgres(..)
       , PostgresT(..)
       , runPostgresT
       , runPG
        -- * Auxiliary types
       , InetText(..)
       , FN(..)
       , textFN
       , MarkedRow(..)
       , mrToBuilder
       , ToMarkedRow(..)
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Base ( MonadBase(..) )
import Control.Monad.Catch
    ( MonadThrow, MonadMask, MonadCatch )
import Control.Monad.Cont.Class ( MonadCont )
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Fix ( MonadFix(..) )
import Control.Monad.HReader
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
import Data.HSet
import Data.Monoid
import Data.Pool
import Data.String
import Data.Text ( Text )
import Data.Typeable
import Database.PostgreSQL.Query.SqlBuilder
    ( ToSqlBuilder(..), SqlBuilder(..) )
import Database.PostgreSQL.Query.TH.SqlExp
    ( sqlExp )
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
    ( FromField(..), typename, returnError )
import Database.PostgreSQL.Simple.ToField
    ( ToField )
import Database.PostgreSQL.Simple.Types
import GHC.Generics
import Instances.TH.Lift ()
import Language.Haskell.TH.Lift ( deriveLift )

import qualified Data.List as L
import qualified Control.Monad.Trans.State.Lazy as STL
import qualified Control.Monad.Trans.State.Strict as STS
import qualified Control.Monad.Trans.Writer.Lazy as WL
import qualified Control.Monad.Trans.Writer.Strict as WS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

{- $setup
>>> import Database.PostgreSQL.Query.SqlBuilder
>>> import Data.Text ( Text )
>>> c <- connect defaultConnectInfo
-}


-- | type to put and get from db 'inet' and 'cidr' typed postgresql
-- fields. This should be in postgresql-simple in fact.
newtype InetText = InetText
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



{- | Dot-separated field name. Each element in nested list will be
properly quoted and separated by dot. It also have instance of
'ToSqlBuilder' and 'IsString` so you can:

>>> let a = "hello" :: FN
>>> a
FN ["hello"]

>>> let b = "user.name" :: FN
>>> b
FN ["user","name"]

>>> let n = "u.name" :: FN
>>> runSqlBuilder c $ toSqlBuilder n
"\"u\".\"name\""

>>> ("user" <> "name") :: FN
FN ["user","name"]

>>> let a = "name" :: FN
>>> let b = "email" :: FN
>>> runSqlBuilder c [sqlExp|^{"u" <> a} = 'name', ^{"e" <> b} = 'email'|]
"\"u\".\"name\" = 'name', \"e\".\"email\" = 'email'"

-}

newtype FN = FN [Text]
    deriving (Ord, Eq, Show, Monoid, Typeable, Generic)

$(deriveLift ''FN)

instance ToSqlBuilder FN where
    toSqlBuilder (FN tt) =
        mconcat
        $ L.intersperse "."
        $ map (toSqlBuilder . Identifier) tt

instance IsString FN where
    fromString s =
        FN
        $ map T.pack
        $ filter (/= ".")
        $ L.groupBy f s
      where
        f a b = not $ a == '.' || b == '.'

{- | Single field to 'FN'

>>> textFN "hello"
FN ["hello"]

>>> textFN "user.name"
FN ["user.name"]

Note that it does not split string to parts by point like instance of
`IsString` does

-}

textFN :: Text -> FN
textFN = FN . (:[])

{- | Marked row is list of pairs of field name and some sql
expression. Used to generate queries like:

@
name = 'name' AND size = 10 AND length = 20
@

or

@
UPDATE tbl SET name = 'name', size = 10, lenght = 20
@

-}

newtype MarkedRow = MR
    { unMR :: [(FN, SqlBuilder)]
    } deriving (Monoid, Typeable, Generic)

class ToMarkedRow a where
    -- | generate list of pairs (field name, field value)
    toMarkedRow :: a -> MarkedRow

instance ToMarkedRow MarkedRow where
    toMarkedRow = id

{- | Turns marked row to query intercalating it with other builder

>>> runSqlBuilder c $ mrToBuilder "AND" $ MR [("name", mkValue "petr"), ("email", mkValue "foo@bar.com")]
" \"name\" = 'petr' AND \"email\" = 'foo@bar.com' "

-}

mrToBuilder :: SqlBuilder        -- ^ Builder to intercalate with
            -> MarkedRow
            -> SqlBuilder
mrToBuilder b (MR l) = mconcat
                       $ L.intersperse b
                       $ map tobld l
  where
    tobld (f, val) = [sqlExp| ^{f} = ^{val} |]


-- | Instances of this typeclass can acquire connection and pass it to
-- computation. It can be reader of pool of connections or just reader of
-- connection

class (MonadBase IO m) => MonadPostgresPool m where
    withPGConnection :: (Connection -> m a) -> m a

instance (MonadPostgresPool m) => MonadPostgresPool (EitherT e m) where
    withPGConnection action = do
        EitherT $ withPGConnection $ \con -> do
            runEitherT $ action con
    {-# INLINABLE withPGConnection #-}

instance (MonadPostgresPool m) => MonadPostgresPool (ExceptT e m) where
    withPGConnection action = do
        ExceptT $ withPGConnection $ \con -> do
            runExceptT $ action con
    {-# INLINABLE withPGConnection #-}

instance (MonadPostgresPool m) => MonadPostgresPool (IdentityT m) where
    withPGConnection action = do
        IdentityT $ withPGConnection $ \con -> do
            runIdentityT $ action con
    {-# INLINABLE withPGConnection #-}

instance (MonadPostgresPool m) => MonadPostgresPool (MaybeT m) where
    withPGConnection action = do
        MaybeT $ withPGConnection $ \con -> do
            runMaybeT $ action con
    {-# INLINABLE withPGConnection #-}

instance (MonadPostgresPool m) => MonadPostgresPool (ReaderT r m) where
    withPGConnection action = do
        ReaderT $ \r -> withPGConnection $ \con ->
            runReaderT (action con) r
    {-# INLINABLE withPGConnection #-}

instance (MonadPostgresPool m) => MonadPostgresPool (STL.StateT s m) where
    withPGConnection action = do
        STL.StateT $ \s -> withPGConnection $ \con ->
            STL.runStateT (action con) s
    {-# INLINABLE withPGConnection #-}

instance (MonadPostgresPool m) => MonadPostgresPool (STS.StateT s m) where
    withPGConnection action = do
        STS.StateT $ \s -> withPGConnection $ \con ->
            STS.runStateT (action con) s
    {-# INLINABLE withPGConnection #-}

instance (MonadPostgresPool m) => MonadPostgresPool (ContT r m) where
    withPGConnection action = do
        ContT $ \r -> withPGConnection $ \con ->
            runContT (action con) r
    {-# INLINABLE withPGConnection #-}

instance (MonadPostgresPool m, Monoid w) => MonadPostgresPool (WL.WriterT w m) where
    withPGConnection action = do
        WL.WriterT $ withPGConnection $ \con ->
            WL.runWriterT (action con)
    {-# INLINABLE withPGConnection #-}

instance (MonadPostgresPool m, Monoid w) => MonadPostgresPool (WS.WriterT w m) where
    withPGConnection action = do
        WS.WriterT $ withPGConnection $ \con ->
            WS.runWriterT (action con)
    {-# INLINABLE withPGConnection #-}

instance (MonadBase IO m, MonadBaseControl IO m, HGettable els (Pool Connection))
         => MonadPostgresPool (HReaderT els m) where
    withPGConnection action = do
        pool <- hask
        withResource pool action

-- | Reader of connection. If you have a
-- connection you can run queries in this monad using 'runPgMonadT'. Or you can
-- use this transformer to run sequence of queries using same connection with
-- 'launchPG'.
newtype PostgresT (level :: N) m a = PostgresT
    { unPostgresT :: ReaderT Connection m a
    } deriving ( Functor, Applicative, Monad , MonadWriter w
               , MonadState s, MonadError e, MonadTrans
               , Alternative, MonadFix, MonadPlus, MonadIO
               , MonadCont, MonadThrow, MonadCatch, MonadMask
               , MonadBase b, MonadLogger )

class (MonadBase IO m) => MonadPostgres m where
  type TransactionLevel m :: N
  askConnection :: m Connection

instance (MonadBase IO m) => MonadPostgres (PostgresT level m) where
  type TransactionLevel (PostgresT level m) = level
  askConnection = PostgresT ask

instance (MonadPostgres m) => MonadPostgres (ReaderT r m) where
  type TransactionLevel (ReaderT r m) = TransactionLevel m
  askConnection = lift askConnection

class ( MonadPostgres m, MonadPostgres n
      , ('S (TransactionLevel m)) ~ TransactionLevel n
      ) => MonadTransaction n m | n -> m, m -> n where
  liftLevel :: n a -> m a

instance ( MonadBase IO m
         ) => MonadTransaction (PostgresT ('S level) m) (PostgresT level m) where
  liftLevel (PostgresT a) = PostgresT a

instance ( MonadTransaction n m
         ) => MonadTransaction (ReaderT r n) (ReaderT r m) where
  liftLevel (ReaderT na) = ReaderT $ liftLevel . na

-- #if MIN_VERSION_monad_control(1,0,0)
-- instance (MonadBaseControl b m) => MonadBaseControl b (PgMonadT m) where
--     type StM (PgMonadT m) a = StM (ReaderT Connection m) a
--     liftBaseWith action = PgMonadT $ do
--         liftBaseWith $ \runInBase -> action (runInBase . unPgMonadT)
--     restoreM st = PgMonadT $ restoreM st
--     {-# INLINABLE liftBaseWith #-}
--     {-# INLINABLE restoreM #-}

-- instance MonadTransControl PgMonadT where
--     type StT PgMonadT a = StT (ReaderT Connection) a
--     liftWith action = PgMonadT $ do
--         liftWith $ \runTrans -> action (runTrans . unPgMonadT)
--     restoreT st = PgMonadT $ restoreT st
--     {-# INLINABLE liftWith #-}
--     {-# INLINABLE restoreT #-}
-- #else
-- instance (MonadBaseControl b m) => MonadBaseControl b (PgMonadT m) where
--     newtype StM (PgMonadT m) a
--         = PgMTM (StM (ReaderT Connection m) a)
--     liftBaseWith action = PgMonadT $ do
--         liftBaseWith $ \runInBase -> do
--             action ((PgMTM `liftM`) . runInBase . unPgMonadT)
--     restoreM (PgMTM st) = PgMonadT $ restoreM st
--     {-# INLINABLE liftBaseWith #-}
--     {-# INLINABLE restoreM #-}

-- instance MonadTransControl PgMonadT where
--     newtype StT PgMonadT a
--         = PgMTT
--           { unPgMTT :: StT (ReaderT Connection) a
--           }
--     liftWith action = PgMonadT $ do
--         liftWith $ \runTrans -> do -- ReaderT Connection n a -> n (StT (ReaderT Connection n) a)
--             action ((PgMTT `liftM`) . runTrans . unPgMonadT)
--     restoreT st = PgMonadT $ restoreT $ unPgMTT `liftM` st
--     {-# INLINABLE liftWith #-}
--     {-# INLINABLE restoreT #-}
-- #endif

-- instance (MonadReader r m) => MonadReader r (PgMonadT m) where
--     ask = lift ask
--     local md ac = do
--         con <- PgMonadT ask
--         lift $ do
--             local md $ runPgMonadT con ac
--     reader = lift . reader
--     {-# INLINABLE ask #-}
--     {-# INLINABLE local #-}
--     {-# INLINABLE reader #-}

-- instance (MonadHReader m) => MonadHReader (PgMonadT m) where
--   type MHRElements (PgMonadT m) = MHRElements m
--   askHSet = PgMonadT askHSet
--   {-# INLINEABLE askHSet #-}

-- instance (MonadBase IO m) => HasPostgres (PgMonadT m) where
--     withPGConnection action = do
--         con <- PgMonadT ask
--         action con
--     {-# INLINABLE withPGConnection #-}

-- instance TransactionSafe (PgMonadT m)


runPostgresT :: Connection -> PostgresT 'Z m a -> m a
runPostgresT con (PostgresT action) = runReaderT action con

{- | If your monad have instance of 'MonadPostgresPool' you maybe dont need this
function, unless your instance use 'withPGPool' which acquires connection
from pool for each query. If you want to run sequence of queries using same
connection you need this function

-}

runPG
  :: (MonadPostgresPool m)
  => PostgresT 'Z m a
  -> m a
runPG act = withPGConnection $ \con -> do
    runPostgresT con act
