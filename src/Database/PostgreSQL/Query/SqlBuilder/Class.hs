module Database.PostgreSQL.Query.SqlBuilder.Class
       ( ToSqlBuilder(..)
       ) where

import Database.PostgreSQL.Query.SqlBuilder.Builder
import Database.PostgreSQL.Simple.Types

-- | Things which always can be transformed to 'SqlBuilder'
class ToSqlBuilder a where
  toSqlBuilder :: a -> SqlBuilder

instance ToSqlBuilder SqlBuilder where
  toSqlBuilder = id

instance ToSqlBuilder Identifier where
  toSqlBuilder ident = mkValue ident

instance ToSqlBuilder QualifiedIdentifier where
  toSqlBuilder qident = mkValue qident
