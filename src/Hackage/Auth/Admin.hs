module Hackage.Auth.Admin
  ( IsAdmin ()
  , isAdmin
  , name
  ) where

import Hackage.Types
import Theory.Named

-- | A proof that the 'Named' @user@ is an administrator. The only way to get a
-- value of this type is via 'isAdmin', after invoking 'name' on a particular
-- 'Username'.
type role IsAdmin nominal
data IsAdmin user = TrustMe

isAdmin :: Named user Username -> IO (Maybe (IsAdmin user))
isAdmin _ = pure $ error "implement isAdmin" TrustMe

