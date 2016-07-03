module Database.PostgreSQL.Query.SqlBuilder
       ( -- * Types
         SqlBuilder(..)
       , ToSqlBuilder(..)
       , Qp(..)
         -- * SqlBuilder helpers
       , emptyB
       , runSqlBuilder
       , mkValue
       , sqlBuilderPure
       , sqlBuilderFromByteString
       , sqlBuilderFromField
       ) where

import Prelude

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
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

{- $setup
>>> c <- connect defaultConnectInfo
-}

-- | Things which always can be transformed to 'SqlBuilder'
class ToSqlBuilder a where
  toSqlBuilder :: a -> SqlBuilder

instance ToSqlBuilder Identifier where
  toSqlBuilder ident = mkValue ident

instance ToSqlBuilder QualifiedIdentifier where
  toSqlBuilder qident = mkValue qident

-- | Special constructor to perform old-style query interpolation
data Qp = forall row. (ToRow row) => Qp Query row

instance ToSqlBuilder Qp where
  toSqlBuilder (Qp q row) = SqlBuilder $ \con _ ->
    pureBuilderResult . BB.fromByteString <$> formatQuery con q row

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

pureBuilderResult :: Builder -> SqlBuilderResult
pureBuilderResult b = SqlBuilderResult b b

data FieldOption

data LogMasker

-- | Builder wich can be effectively concatenated. Requires 'Connection'
-- inside for string quoting implemented in __libpq__. Builds two strings: query
-- string and log string which may differ.
newtype SqlBuilder = SqlBuilder
  { sqlBuild :: Connection -> LogMasker -> IO SqlBuilderResult
  } deriving (Typeable, Generic)

instance Monoid SqlBuilder where
  mempty = sqlBuilderPure mempty
  mappend (SqlBuilder a) (SqlBuilder b) =
    SqlBuilder $ \c masker -> mappend <$> (a c masker) <*> (b c masker)


-- | Typed synonym of 'mempty'
emptyB :: SqlBuilder
emptyB = mempty

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

instance IsString SqlBuilder where
  fromString s = SqlBuilder $ \_ _ -> return $ pureBuilderResult $ BB.fromString s

instance ToSqlBuilder SqlBuilder where
  toSqlBuilder = id


{- | Function to convert single field value to builder

>>> runSqlBuilder c $ mkValue "some ' value"
"'some '' value'"

Note correct string quoting
-}

mkValue :: (ToField a) => a -> SqlBuilder
mkValue a = error "FIXME: not implemented"


-- | Lift pure bytestring builder to 'SqlBuilder'. This is unsafe to use
-- directly in your code.
sqlBuilderPure :: Builder -> SqlBuilder
sqlBuilderPure b = SqlBuilder $ \_ _ -> pure $ pureBuilderResult b

-- | Unsafe function to make SqlBuilder from arbitrary ByteString. Does not
-- perform any checks
sqlBuilderFromByteString :: ByteString -> SqlBuilder
sqlBuilderFromByteString = sqlBuilderPure . BB.fromByteString

sqlBuilderFromField :: (ToField a) => FieldOption -> a -> SqlBuilder
sqlBuilderFromField = error "FIXME: not implemented"
