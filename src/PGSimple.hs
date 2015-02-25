module PGSimple
       ( -- * Common usage modules
         module PGSimple.Entity
       , module PGSimple.Functions
       , module PGSimple.SqlBuilder
       , module PGSimple.TH
       , module PGSimple.Types

           -- * Some re-exports from postgresql-simple
       , Connection, connect, defaultConnectInfo, connectPostgreSQL
       , ConnectInfo(..) , ToField(..), ToRow(..), FromField(..)
       , FromRow(..), Query(..), Only(..), In(..), Oid(..), Values(..)
       , (:.)(..), PGArray(..), HStoreList(..), HStoreMap(..)
       , ToHStore(..), HStoreBuilder , hstore, parseHStoreList
       , ToHStoreText(..), HStoreText , sqlQQ
       ) where


import Database.PostgreSQL.Simple
    ( ToRow, Connection, FromRow, defaultConnectInfo,
      connectPostgreSQL, connect, ConnectInfo(..) )
import Database.PostgreSQL.Simple.FromField ( FromField(..) )
import Database.PostgreSQL.Simple.FromRow ( FromRow(..) )
import Database.PostgreSQL.Simple.HStore hiding
    ( toBuilder, toLazyByteString ) -- to prevent conflicts
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField ( ToField(..) )
import Database.PostgreSQL.Simple.ToRow ( ToRow(..) )
import Database.PostgreSQL.Simple.Types
import Language.Haskell.TH.Quote ( QuasiQuoter )

import PGSimple.Entity
import PGSimple.Functions
import PGSimple.SqlBuilder
import PGSimple.TH
import PGSimple.Types

sqlQQ :: QuasiQuoter
sqlQQ = sql

{-# DEPRECATED sqlQQ "Use 'sqlExp' instead" #-}
