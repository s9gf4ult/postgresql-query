module Database.PostgreSQL.Query.SqlBuilder.Types
       ( -- * Sql builder result
         SqlBuilderResult(..)
       , builderResultPure
         -- * Field masking in logs
       , FieldOption(..)
       , LogMasker
       , defaultLogMasker
       , hugeFieldsMasker
       ) where


import Blaze.ByteString.Builder (Builder)
import Data.Semigroup
import Data.String
import Data.Typeable
import GHC.Generics (Generic)

import qualified Blaze.ByteString.Builder as BB
import qualified Data.ByteString as BS

-- | Result if SqlBuilder. Contains separated builder for query and log.
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

-- | Option for field instructing 'LogMasker' what to do with field when logging
data FieldOption
  = FieldDefault
    -- ^ Do nothing. Field should be pasted as is
  | FieldMasked
    -- ^ Mask field in logs with placeholder.
  deriving (Eq, Ord, Show, Typeable, Generic)

-- | Function modifying query parameter value before pasting it to log. Returns
-- Nothing if query argument should be passed to log as is.
type LogMasker = FieldOption -> Builder -> Builder

-- | Simply replaces masked fields with placeholder.
defaultLogMasker :: LogMasker
defaultLogMasker FieldDefault bb = bb
defaultLogMasker FieldMasked _  = "'<MASKED BY POSTGRESQL-QUERY>'"

-- | Masks fields which size is bigger than given argument in bytes.
hugeFieldsMasker :: Int -> LogMasker
hugeFieldsMasker maxsize _ bb =
  let bl = BS.length $ BB.toByteString bb
  in if bl > maxsize
     then fromString $ "'<STRING SIZE: " ++ show bl ++ " MASKED BY POSTGRESQL-QUERY>'"
     else bb
