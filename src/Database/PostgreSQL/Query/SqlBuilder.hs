module Database.PostgreSQL.Query.SqlBuilder
       ( module Database.PostgreSQL.Query.SqlBuilder.Builder
       , module Database.PostgreSQL.Query.SqlBuilder.Class
       , module Database.PostgreSQL.Query.SqlBuilder.Types
       ) where

import Database.PostgreSQL.Query.SqlBuilder.Builder
import Database.PostgreSQL.Query.SqlBuilder.Class
import Database.PostgreSQL.Query.SqlBuilder.Types

-- -- | Special constructor to perform old-style query interpolation
-- data Qp = forall row. (ToRow row) => Qp Query row

-- instance ToSqlBuilder Qp where
--   toSqlBuilder (Qp q row) = SqlBuilder $ \con _ ->
--     builderResultPure . BB.fromByteString <$> formatQuery con q row
