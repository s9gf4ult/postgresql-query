module PGSimple.Helpers where

import Prelude

import Blaze.ByteString.Builder ( Builder )
import Blaze.ByteString.Builder.ByteString ( fromByteString )
import Control.Applicative
import Control.Exception
import Data.ByteString ( ByteString )
import Data.Monoid
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Builder as TBL

newtype SqlBuilder =
    SqlBuilder
    { sqlBuild :: Connection -> IO Builder }

sqlBuilderPure :: Builder -> SqlBuilder
sqlBuilderPure b = SqlBuilder $ const $ pure b

sqlBuilderBS :: ByteString -> SqlBuilder
sqlBuilderBS bs = sqlBuilderPure $ fromByteString bs

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


quoteOrThrow :: Query -> Either ByteString ByteString -> Builder
quoteOrThrow q = either (throwFormatError q) (inQuotes . fromByteString)

-- | Shity copy paste from postgresql-simple
buildAction :: Connection -> Query -> Action -> IO Builder
buildAction _ _ (Plain b)            = pure b
buildAction c q (Escape s)           = quoteOrThrow q <$> escapeStringConn c s
buildAction c q (EscapeByteA s)      = quoteOrThrow q <$> escapeByteaConn c s
buildAction c q (EscapeIdentifier s) = either (throwFormatError q) fromByteString
                                     <$> escapeIdentifier c s
buildAction c q (Many  ys)           = mconcat <$> mapM (buildAction c q) ys
