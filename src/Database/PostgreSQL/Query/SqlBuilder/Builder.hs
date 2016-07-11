module Database.PostgreSQL.Query.SqlBuilder.Builder
       ( SqlBuilder(..)
         -- * Running
       , runSqlBuilder
         -- * Building
       , emptyB
       , mkValue
       , mkMaskedValue
       , sqlBuilderFromField
       -- ** Unsafe
       , sqlBuilderPure
       , sqlBuilderFromByteString
       ) where

import Blaze.ByteString.Builder (Builder)
import Data.ByteString (ByteString)
import Data.Semigroup
import Data.String
import Data.Typeable
import Database.PostgreSQL.Query.SqlBuilder.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Internal
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types
import GHC.Generics (Generic)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import qualified Blaze.ByteString.Builder as BB
import qualified Blaze.ByteString.Builder.Char.Utf8 as BB


-- | Builder wich can be effectively concatenated. Requires 'Connection'
-- inside for string quoting implemented in __libpq__. Builds two strings: query
-- string and log string which may differ.
newtype SqlBuilder = SqlBuilder
  { sqlBuild :: Connection -> LogMasker -> IO SqlBuilderResult
  } deriving (Typeable, Generic)

instance Semigroup SqlBuilder where
  (SqlBuilder a) <> (SqlBuilder b) =
    SqlBuilder $ \c masker -> (<>) <$> (a c masker) <*> (b c masker)

instance Monoid SqlBuilder where
  mempty = sqlBuilderPure mempty
  mappend = (<>)

instance IsString SqlBuilder where
  fromString s = SqlBuilder $ \_ _ -> return $ builderResultPure $ BB.fromString s

-- | Returns query string with log bytestring
runSqlBuilder :: Connection -> LogMasker -> SqlBuilder -> IO (Query, ByteString)
runSqlBuilder con masker (SqlBuilder bld) = toTuple <$> bld con masker
  where
    toTuple res =
      ( Query $ BB.toByteString $ sbQueryString res
      , BB.toByteString $ sbLogString res )

-- | Typed synonym of 'mempty'
emptyB :: SqlBuilder
emptyB = mempty

-- | Shorthand function to convert single field value to builder
mkValue :: (ToField a) => a -> SqlBuilder
mkValue = sqlBuilderFromField FieldDefault

-- | Shorthand function to convert single masked field value (which should not
-- be shown in log)
mkMaskedValue :: (ToField a) => a -> SqlBuilder
mkMaskedValue = sqlBuilderFromField FieldMasked

sqlBuilderFromField :: (ToField a) => FieldOption -> a -> SqlBuilder
sqlBuilderFromField fo field = SqlBuilder $ \con masker -> do
  qbs <- buildAction con "" [] $ toField field
  let sbQueryString = qbs
      sbLogString   = masker fo qbs
  return SqlBuilderResult{..}

-- | Lift pure bytestring builder to 'SqlBuilder'. This is unsafe to use
-- directly in your code.
sqlBuilderPure :: Builder -> SqlBuilder
sqlBuilderPure b = SqlBuilder $ \_ _ -> pure $ builderResultPure b

-- | Unsafe function to make SqlBuilder from arbitrary ByteString. Does not
-- perform any checks. Dont use it directly in your code unless you know what
-- you are doing.
sqlBuilderFromByteString :: ByteString -> SqlBuilder
sqlBuilderFromByteString = sqlBuilderPure . BB.fromByteString
