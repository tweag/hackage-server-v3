{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Servant.HackageCombinators.NegotiableContent
  ( NegotiableContent
  , NegotiatedContent(..)
  ) where

import Control.Lens (over, _last, preview, (%~), (&))
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Text.Lens (unpacked)
import Network.HTTP.Types (hAccept)
import Network.Mime (MimeType, defaultMimeLookup)
import Network.Wai (Request, requestHeaders, pathInfo)
import Servant.API
import Servant.Internal.Links
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

newtype NegotiatedContent = NegotiatedContent
  { negotiatedExtention :: T.Text
  }

instance HasLink api => HasLink (NegotiableContent :> api) where
  type MkLink (NegotiableContent :> api) a = Maybe NegotiatedContent -> MkLink api a
  toLink toA _ l (Just (NegotiatedContent v)) =
    toLink
      (toA . (\(Link a b c) -> Link (a & _last %~ escaped . (\z -> z <> "." <> T.unpack v) . getEscaped) b c))
      (Proxy :: Proxy api)
      l
  toLink toA _ l Nothing = toLink toA (Proxy :: Proxy api) l

instance HasServer api context => HasServer (NegotiableContent :> api) context where
  type ServerT (NegotiableContent :> api) m = Maybe NegotiatedContent -> ServerT api m
  hoistServerWithContext _ p f m = hoistServerWithContext (Proxy @api) p f . m
  route _ ctx app = RawRouter $ \env req respond -> do
    let (content, req') = negotiateContentFromExtension req
    runRouterEnv
      (notFoundErrorFormatter defaultErrorFormatters)
      (route (Proxy @api) ctx $ fmap ($ content) app)
      env
      req'
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
negotiateContentFromExtension :: Request -> (Maybe NegotiatedContent, Request)
negotiateContentFromExtension req = fromMaybe (Nothing, req) $ do
  lastSeg <- preview _last $ pathInfo req
  mime <- extensionToMime lastSeg
  pure $ (Just (NegotiatedContent lastSeg),)
    req
      { requestHeaders
          = (hAccept, mime)
          : filter ((/= hAccept) . fst) (requestHeaders req)
      , pathInfo
          = over (_last . unpacked) dropExtensions $ pathInfo req
      }

