{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wno-orphans   #-}

module Servant.HackageCombinators.HackageAuth where

import Data.Pool (Pool)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Hackage.Types
import Hackage.Utils (Connection)
import Network.Wai (Request)
import Servant.API
import Servant.HackageAuth (RealmName, authErrorResponse, hackageRealm, checkAuthenticated)
import Servant.Server hiding (respond)
import Servant.Server.Experimental.Auth


-- | Custom combinator for doing the same auth checks as Hackage v2.
type HackageAuth = AuthProtect "hackage-auth"
type instance AuthServerData HackageAuth = UserId


-- | Generate an 'AuthHandler' for 'HackageAuth' auth.
hackageAuthHandler :: RealmName -> Pool Connection -> AuthHandler Request UserId
hackageAuthHandler realm conn = mkAuthHandler $ \req -> Handler $ ExceptT $ do
  eauth <- runExceptT $ checkAuthenticated hackageRealm conn req
  case eauth of
    Left err -> fmap Left $ authErrorResponse realm err
    Right a -> pure $ pure a

