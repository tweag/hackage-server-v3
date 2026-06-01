module Hackage.Auth.Maintainer
  ( IsMaintainer ()
  , isMaintainer
  , name
  ) where

import Hackage.Types
import Theory.Named

-- | A proof that the 'Named' @user@ is a package maintainer. The only way to get a
-- value of this type is via 'isMaintainer', after invoking 'name' on a particular
-- 'Username' and 'PackageName'.
type role IsMaintainer nominal nominal
data IsMaintainer user package = TrustMe

isMaintainer
  :: Named user UserName
  -> Named package PackageName
  -> IO (Maybe (IsMaintainer user package))
isMaintainer _ = pure $ error "implement isMaintainer" TrustMe


