module Database.PostgreSQL.Query.Entity.Class
       ( Entity(..)
       , Ent
       ) where

import Database.PostgreSQL.Query.Import
import Database.PostgreSQL.Query.Types

-- | Auxiliary typeclass for data types which can map to rows of some
-- table. This typeclass is used inside functions like 'pgSelectEntities' to
-- generate queries.
class Entity a where
    -- | Id type for this entity
#if MIN_VERSION_base(4, 9, 0)
    data EntityId a :: Type
#else
    data EntityId a :: *
#endif
    -- | Table name of this entity
    tableName :: Proxy a -> FN
    -- | Field names without 'id' and 'created'. The order of field names must match
    -- with order of fields in 'ToRow' and 'FromRow' instances of this type.
    fieldNames :: Proxy a -> [FN]

deriving instance Typeable EntityId

-- | Entity with it's id
type Ent a = (EntityId a, a)
