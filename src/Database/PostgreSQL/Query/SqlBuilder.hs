module Database.PostgreSQL.Query.SqlBuilder
       ( -- * SqlBuilder
         SqlBuilder(..)
       , runSqlBuilder
         -- ** Creating SqlBuilder
       , emptyB
       , mkValue
         -- *** Unsafe
       , sqlBuilderPure
       , sqlBuilderFromByteString
       , sqlBuilderFromField
         -- ** Class
       , ToSqlBuilder(..)
         -- * LogMasker
       , LogMasker
       , defaultLogMasker
       , hugeFieldsBuilder
         -- * FieldOption
       , FieldOption(..)
         -- * SqlBuilderResult
       , SqlBuilderResult(..)
       , builderResultPure
         -- * Old style query interpolation
       , Qp(..)
       ) where

import Blaze.ByteString.Builder
    ( Builder, toByteString )
import Control.Applicative
import Data.ByteString ( ByteString )
import Data.Semigroup
import Data.String
import Data.Text ( Text )
import Data.Typeable ( Typeable )
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Internal
    ( buildAction )
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types
import GHC.Generics ( Generic )

import qualified Blaze.ByteString.Builder.ByteString as BB
import qualified Blaze.ByteString.Builder.Char.Utf8 as BB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

{- $setup
>>> c <- connect defaultConnectInfo
-}

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

instance ToSqlBuilder SqlBuilder where
  toSqlBuilder = id

{- | Performs parameters interpolation and return ready to execute query

>>> let val = 10
>>> let name = "field"
>>> runSqlBuilder c $ "SELECT * FROM tbl WHERE " <> mkIdent name <> " = " <> mkValue val
"SELECT * FROM tbl WHERE \"field\" = 10"

-}

-- | Returns query string with log bytestring
runSqlBuilder :: Connection -> LogMasker -> SqlBuilder -> IO (Query, ByteString)
runSqlBuilder con masker (SqlBuilder bld) = toTuple <$> bld con masker
  where
    toTuple res = ( Query $ toByteString $ sbQueryString res
                  , toByteString $ sbLogString res )

-- | Typed synonym of 'mempty'
emptyB :: SqlBuilder
emptyB = mempty

{- | Shorthand function to convert single field value to builder

>>> runSqlBuilder c $ mkValue "some ' value"
"'some '' value'"

Note correct string quoting
-}

mkValue :: (ToField a) => a -> SqlBuilder
mkValue = sqlBuilderFromField FieldDefault

-- | Shorthand function to convert single masked field value (which should not
-- be shown in log)
mkMaskedValue :: (ToField a) => a -> SqlBuilder
mkMaskedValue = sqlBuilderFromField FieldMasked

-- | Lift pure bytestring builder to 'SqlBuilder'. This is unsafe to use
-- directly in your code.
sqlBuilderPure :: Builder -> SqlBuilder
sqlBuilderPure b = SqlBuilder $ \_ _ -> pure $ builderResultPure b

-- | Unsafe function to make SqlBuilder from arbitrary ByteString. Does not
-- perform any checks
sqlBuilderFromByteString :: ByteString -> SqlBuilder
sqlBuilderFromByteString = sqlBuilderPure . BB.fromByteString

sqlBuilderFromField :: (ToField a) => FieldOption -> a -> SqlBuilder
sqlBuilderFromField fo field = SqlBuilder $ \con masker -> do
  qbs <- buildAction con "" [] $ toField field
  let sbQueryString = qbs
      sbLogString   = masker fo qbs
  return SqlBuilderResult{..}


-- | Things which always can be transformed to 'SqlBuilder'
class ToSqlBuilder a where
  toSqlBuilder :: a -> SqlBuilder

instance ToSqlBuilder Identifier where
  toSqlBuilder ident = mkValue ident

instance ToSqlBuilder QualifiedIdentifier where
  toSqlBuilder qident = mkValue qident

-- | Function modifying query parameter value before pasting it to log. Returns
-- Nothing if query argument should be passed to log as is.
type LogMasker = FieldOption -> Builder -> Builder

-- | Simply replaces masked fields with placeholder.
defaultLogMasker :: LogMasker
defaultLogMasker FieldDefault bb = bb
defaultLogMasker FieldMasked _  = "'<MASKED BY POSTGRESQL-QUERY>'"

-- | Masks fields which size is bigger than given argument in bytes.
hugeFieldsBuilder :: Int -> LogMasker
hugeFieldsBuilder maxsize _ bb =
  let bl = BS.length $ toByteString bb
  in if bl > maxsize
     then fromString $ "'<STRING SIZE: " ++ show bl ++ " MASKED BY POSTGRESQL-QUERY>'"
     else bb

-- | Option for field instructing 'LogMasker' what to do with field when logging
data FieldOption
  = FieldDefault
    -- ^ Do nothing. Field should be pasted as is
  | FieldMasked
    -- ^ Mask field in logs with placeholder.
  deriving (Eq, Ord, Show, Typeable, Generic)

data SqlBuilderResult = SqlBuilderResult
  { sbQueryString :: Builder
  , sbLogString   :: Builder
  } deriving (Typeable, Generic)

instance Semigroup SqlBuilderResult where
  (SqlBuilderResult a b) <> (SqlBuilderResult a' b') =
    SqlBuilderResult (a <> a') (b <> b')

instance Monoid SqlBuilderResult where
  mempty  = SqlBuilderResult mempty mempty
  mappend = (<>)

builderResultPure :: Builder -> SqlBuilderResult
builderResultPure b = SqlBuilderResult b b

-- | Special constructor to perform old-style query interpolation
data Qp = forall row. (ToRow row) => Qp Query row

instance ToSqlBuilder Qp where
  toSqlBuilder (Qp q row) = SqlBuilder $ \con _ ->
    builderResultPure . BB.fromByteString <$> formatQuery con q row
