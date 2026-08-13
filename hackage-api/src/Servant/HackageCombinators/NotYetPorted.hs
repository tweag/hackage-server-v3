{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Servant.HackageCombinators.NotYetPorted where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (runResourceT)
import Network.HTTP.Client (Manager)
import Network.HTTP.ReverseProxy
import Network.HTTP.Types.Header (hHost)
import Network.Wai (requestHeaders)
import Servant.EDE
import Servant.Server hiding (respond)
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.Router


-- | Custom combinator for routes that are not yet ported to v3; in these cases,
-- the routing framework will automatically reverse proxy Hackage v2.
data NotYetPorted = NotYetPorted

instance HasContextEntry context Manager => HasServer NotYetPorted context where
  type ServerT NotYetPorted m = NotYetPorted
  hoistServerWithContext _ _ _ = id
  route _ ctx application = RawRouter $ \env req respond ->
    waiProxyTo
      forwardRequest
      defaultOnExc
      (getContextEntry ctx)
      req $ \resp -> runResourceT $ do
        delayed <- runDelayed application env req
        liftIO $ respond $ resp <$ delayed
    where
      forwardRequest req =
        pure
          $ WPRModifiedRequestSecure
              req
                { requestHeaders
                    = (hHost, "hackage.haskell.org")
                    : filter ((/= hHost) . fst) (requestHeaders req)
                }
          $ ProxyDest "hackage.haskell.org" 443

instance TemplateFiles NotYetPorted where
  templateFiles = mempty



