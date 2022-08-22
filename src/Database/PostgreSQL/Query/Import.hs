-- | Module for internal usage. Just reexports and some compatibility
-- stuff

module Database.PostgreSQL.Query.Import
  ( module X
  ) where

import Control.Applicative as X
import Control.Monad as X
import Control.Monad.Base as X
import Control.Monad.Catch as X
import Control.Monad.Logger.CallStack as X
import Control.Monad.Trans.Control as X
import Data.List.NonEmpty as X (NonEmpty)
import Data.Maybe as X
import Data.Proxy as X
import Data.String as X
import Data.Text as X (Text)
import Data.Typeable as X
import GHC.Generics as X (Generic)
import GHC.Stack as X

#if MIN_VERSION_base(4,8,0)
import Data.Semigroup as X
#else
import Data.Monoid as X
import Data.Semigroup as X
#endif

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail as X
#endif

#if MIN_VERSION_base(4, 9, 0)
import Data.Kind as X
#endif
