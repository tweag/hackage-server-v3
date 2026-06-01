module Hackage.Auth.Trustee
  ( IsTrustee ()
  , isTrustee
  , name
  ) where

import Hackage.Types
import Theory.Named

-- | A proof that the 'Named' @user@ is a hackage trustee. The only way to get a
-- value of this type is via 'isTrustee', after invoking 'name' on a particular
-- 'Username'.
type role IsTrustee nominal
data IsTrustee user = TrustMe

isTrustee :: Named user UserName -> IO (Maybe (IsTrustee user))
isTrustee _ = pure $ error "implement isTrustee" TrustMe

