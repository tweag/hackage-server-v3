{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.HackageCombinators.UserDomain where

import Control.Applicative (asum)
import Data.ByteString (ByteString)
import Data.Proxy (Proxy (..))
import Network.HTTP.Types (hLocation)
import Network.Wai (rawPathInfo, requestHeaderHost, requestHeaders)
import Servant.API
import Servant.Server hiding (respond)
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.DelayedIO


-- | Servant combinator for automatically redirecting traffic to a different
-- domain. The idea here is to have both domains reverse proxied to the same
-- server, and then use this combinator to force which one serves the content.
-- This is desirable when serving user content and not needing to worry about
-- XSS.
--
-- Note that we use the 'UserDomain' type both at the API level, and in the
-- context to provide the actual user domain to redirect to.
newtype UserDomain = UserDomain
  { getUserDomain :: ByteString
  }

instance ( HasServer api context
         , HasContextEntry context UserDomain
         ) => HasServer (UserDomain :> api) context where
  type ServerT (UserDomain :> api) m = ServerT api m
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)
  route _ ctx app =
    route (Proxy @api) ctx $
      addHeaderCheck (fmap const app)
        $ withRequest $ \req -> do
            let userDomain = getUserDomain $ getContextEntry ctx
                -- Try both the 'requestHeaderHost' and the @Host@ header
                -- directly; these come from different code paths, and the
                -- former doesn't work in our test suite.
                host = asum
                  [ requestHeaderHost req
                  , lookup ("Host") $ requestHeaders req
                  ]
            case (host == Just userDomain) of
              True -> pure ()
              False ->
                delayedFailFatal $ err301
                  { errHeaders = [(hLocation, userDomain <> rawPathInfo req)]
                  }

