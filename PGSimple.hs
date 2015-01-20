module PGSimple
       ( -- * Common usage modules
         module PGSimple.Types
       , module PGSimple.Functions
           -- * Some re-exports from postgresql-simple
       , Connection, connect, defaultConnectInfo, connectPostgreSQL
       , ToField(..), ToRow(..), FromField(..), FromRow(..)
       , Query(..), Only(..), In(..), Oid(..), Values(..)
       , (:.)(..), PGArray(..) , HStoreList(..), HStoreMap(..)
       , ToHStore(..), HStoreBuilder , hstore, parseHStoreList
       , ToHStoreText(..), HStoreText , sqlQQ
       ) where


import Database.PostgreSQL.Simple
    ( ToRow, Connection, FromRow, defaultConnectInfo,
      connectPostgreSQL, connect )
import Database.PostgreSQL.Simple.FromField ( FromField(..) )
import Database.PostgreSQL.Simple.FromRow ( FromRow(..) )
import Database.PostgreSQL.Simple.HStore hiding
    ( toBuilder, toLazyByteString ) -- to prevent conflicts
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField ( ToField(..) )
import Database.PostgreSQL.Simple.ToRow ( ToRow(..) )
import Database.PostgreSQL.Simple.Types
import Language.Haskell.TH.Quote ( QuasiQuoter )

import PGSimple.Types
import PGSimple.Functions


sqlQQ :: QuasiQuoter
sqlQQ = sql
