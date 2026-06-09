{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Servant.HackageCombinators.PermanentRedirect where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (runResourceT)
import Data.Functor ((<&>))
import Network.HTTP.Types.Header (hLocation)
import Network.HTTP.Types.Status (permanentRedirect308)
import Network.Wai (responseLBS)
import Servant.API
import Servant.EDE
import Servant.Server hiding (respond)
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.Router

-- | A permanent redirect (HTTP 308) to somewhere else in the app. Hackage v2's
-- router was very permissive about empty URL segments, so in order to provide
-- backwards compatability, we 308 them to their new canonical URIs.
data PermanentRedirect

instance HasServer PermanentRedirect context where
  type ServerT PermanentRedirect m = Link
  hoistServerWithContext _ _ _ = id
  route _ _ tlink = RawRouter $ \env req resp -> runResourceT $ do
    delayed <- runDelayed tlink env req
    liftIO $ resp $ delayed <&> \link ->
      responseLBS permanentRedirect308 [(hLocation, "/" <> toHeader link)] mempty

instance TemplateFiles PermanentRedirect where
  templateFiles = mempty

