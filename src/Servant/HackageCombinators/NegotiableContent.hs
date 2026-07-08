{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Servant.HackageCombinators.NegotiableContent (NegotiableContent) where

import Control.Lens (over, _last, preview)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Text.Lens (unpacked)
import Network.HTTP.Types (hAccept)
import Network.Mime (MimeType, defaultMimeLookup)
import Network.Wai (Request, requestHeaders, pathInfo)
import Servant.API
import Servant.Server hiding (respond)
import Servant.Server.Internal.Router
import System.FilePath (dropExtensions)


-- | Endpoints whose content can be negotiated by giving a file extension. Eg,
-- you can request @text/json@ by appending @.json@ to the URI. For example:
--
-- @NegotiableContent :> "resource" :> Get '[HTML, JSON] ...@
--
-- will match routes @/resource@, @/resource.html@, and @/resource.json@. It
-- will use standard content negotiation for the @/resource@ route.
data NegotiableContent

instance HasLink api => HasLink (NegotiableContent :> api) where
  type MkLink (NegotiableContent :> api) a = MkLink api a
  toLink f _ = toLink f (Proxy @api)

instance HasServer api context => HasServer (NegotiableContent :> api) context where
  type ServerT (NegotiableContent :> api) m = ServerT api m
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)
  route _ ctx app = RawRouter $ \env req respond ->
    runRouterEnv
      (notFoundErrorFormatter defaultErrorFormatters)
      (route (Proxy @api) ctx app)
      env
      (negotiateContentFromExtension req)
      respond


-- | Given a URL segment, attempt to extract a mimetype from an extension (if it
-- has one.)
extensionToMime :: T.Text -> Maybe MimeType
extensionToMime segment =
  case T.isInfixOf "." segment of
    False -> Nothing
    True -> Just $ defaultMimeLookup segment


-- | Parse the extension off the last segment of a 'Request', remove it, and set
-- the @Accept@ header to be the associated mimetype. This allows us to
-- negotiate a content type based on an extension.
negotiateContentFromExtension :: Request -> Request
negotiateContentFromExtension req = fromMaybe req $ do
  lastSeg <- preview _last $ pathInfo req
  mime <- extensionToMime lastSeg
  pure $
    req
      { requestHeaders
          = (hAccept, mime)
          : filter ((/= hAccept) . fst) (requestHeaders req)
      , pathInfo
          = over (_last . unpacked) dropExtensions $ pathInfo req
      }

