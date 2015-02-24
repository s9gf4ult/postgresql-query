module PGSimple.SqlBuilder
       ( -- * Types
         SqlBuilder(..)
       , ToSqlBuilder(..)
       , Qp(..)
         -- * SqlBuilder helpers
       , emptyB
       , runSqlBuilder
       , mkIdent
       , mkValue
       , sqlBuilderPure
       , sqlBuilderFromField
       ) where

import Prelude

import Blaze.ByteString.Builder
    ( Builder, toByteString )
import Control.Applicative
import Control.Exception
import Data.ByteString ( ByteString )
import Data.Monoid
import Data.String
import Data.Text ( Text )
import Data.Typeable ( Typeable )
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types
    ( Query(..), Identifier(..) )
import GHC.Generics ( Generic )

import qualified Blaze.ByteString.Builder.ByteString as BB
import qualified Blaze.ByteString.Builder.Char.Utf8 as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL

{- $setup
>>> c <- connect defaultConnectInfo
-}

-- | Things which always can be transformed to 'SqlBuilder'
class ToSqlBuilder a where
    toSqlBuilder :: a -> SqlBuilder

-- | Special constructor to perform old-style query interpolation
data Qp = forall row. (ToRow row) => Qp Query row

instance ToSqlBuilder Qp where
    toSqlBuilder (Qp q row) = SqlBuilder $ \c ->
        BB.fromByteString <$> formatQuery c q row


-- | Builder wich can be effectively concatenated. Requires 'Connection'
-- inside for string quoting implemented in __libpq__
newtype SqlBuilder =
    SqlBuilder
    { sqlBuild :: Connection -> IO Builder }
    deriving (Typeable, Generic)

-- | Typed synonym of 'mempty'
emptyB :: SqlBuilder
emptyB = mempty

{- | Performs parameters interpolation and return ready to execute query

>>> let val = 10
>>> let name = "field"
>>> runSqlBuilder c $ "SELECT * FROM tbl WHERE " <> mkIdent name <> " = " <> mkValue val
"SELECT * FROM tbl WHERE \"field\" = 10"

-}

runSqlBuilder :: Connection -> SqlBuilder -> IO Query
runSqlBuilder con (SqlBuilder bld) =
    (Query . toByteString) <$> bld con

instance IsString SqlBuilder where
    fromString s =
        let b = fromString s :: ByteString
        in toSqlBuilder b

instance ToSqlBuilder SqlBuilder where
    toSqlBuilder = id
instance ToSqlBuilder Builder where
    toSqlBuilder = sqlBuilderPure
instance ToSqlBuilder ByteString where
    toSqlBuilder = sqlBuilderPure . BB.fromByteString
instance ToSqlBuilder BL.ByteString where
    toSqlBuilder = sqlBuilderPure . BB.fromLazyByteString
instance ToSqlBuilder String where
    toSqlBuilder = sqlBuilderPure . BB.fromString
instance ToSqlBuilder T.Text where
    toSqlBuilder = sqlBuilderPure . BB.fromText
instance ToSqlBuilder TL.Text where
    toSqlBuilder = sqlBuilderPure . BB.fromLazyText

{- | Shorthand function to convert identifier name to builder

>>> runSqlBuilder c $ mkIdent "simple\"ident"
"\"simple\"\"ident\""

Note correct string quoting made by __libpq__
-}

mkIdent :: Text -> SqlBuilder
mkIdent t = sqlBuilderFromField "mkident a" $ Identifier t

{- | Shorthand function to convert single value to builder

>>> runSqlBuilder c $ mkValue "some ' value"
"'some '' value'"

Note correct string quoting
-}

mkValue :: (ToField a) => a -> SqlBuilder
mkValue a = sqlBuilderFromField "mkValue a" a

-- | Lift pure bytestring builder to 'SqlBuilder'
sqlBuilderPure :: Builder -> SqlBuilder
sqlBuilderPure b = SqlBuilder $ const $ pure b

sqlBuilderFromField :: (ToField a) => Query -> a -> SqlBuilder
sqlBuilderFromField q a =
    SqlBuilder $ \c -> buildAction c q $ toField a

instance Monoid SqlBuilder where
    mempty = sqlBuilderPure mempty
    mappend (SqlBuilder a) (SqlBuilder b) =
        SqlBuilder $ \c -> mappend <$> (a c) <*> (b c)

throwFormatError :: Query -> ByteString -> a
throwFormatError q msg = throw
             $ FormatError
               { fmtMessage = utf8ToString msg
               , fmtQuery = q
               , fmtParams = [] --  FIXME: Maybe paste something here
               }
  where
    utf8ToString = T.unpack . T.decodeUtf8

-- | for internal usage
quoteOrThrow :: Query -> Either ByteString ByteString -> Builder
quoteOrThrow q = either (throwFormatError q) (inQuotes . BB.fromByteString)

-- | Shity copy-paste from postgresql-simple
buildAction :: Connection -> Query -> Action -> IO Builder
buildAction _ _ (Plain b)            = pure b
buildAction c q (Escape s)           = quoteOrThrow q <$> escapeStringConn c s
buildAction c q (EscapeByteA s)      = quoteOrThrow q <$> escapeByteaConn c s
buildAction c q (EscapeIdentifier s) = either (throwFormatError q) BB.fromByteString
                                     <$> escapeIdentifier c s
buildAction c q (Many  ys)           = mconcat <$> mapM (buildAction c q) ys
