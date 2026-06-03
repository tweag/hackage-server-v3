{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Servant.HackageCombinators where

import Control.Lens (over, _last, preview)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (runResourceT)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Text.Lens (unpacked)
import Hackage.Types
import Hackage.Utils (Connection)
import Network.HTTP.Client (Manager)
import Network.HTTP.ReverseProxy
import Network.HTTP.Types   (hAccept)
import Network.HTTP.Types.Header (hHost, hLocation)
import Network.HTTP.Types.Status (permanentRedirect308)
import Network.Mime (MimeType, defaultMimeLookup)
import Network.Wai (Request, requestHeaders, pathInfo, responseLBS)
import Servant.API
import Servant.EDE
import Servant.HackageAuth (RealmName, authErrorResponse, hackageRealm, checkAuthenticated)
import Servant.Server hiding (respond)
import Servant.Server.Experimental.Auth
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.Router
import System.FilePath (dropExtensions)
import Data.Kind (Type)
import GHC.TypeLits


-- | A 'Capture'-able segment corresponding to hackage v2's @.:format@
data AnyFormat = AnyFormat

instance FromHttpApiData AnyFormat where
  parseUrlPiece t =
    case T.isPrefixOf "." t of
      True -> pure AnyFormat
      False -> Left $ "Invalid AnyFormat: " <> t

instance ToHttpApiData AnyFormat where
  toUrlPiece _ = ".dummy"


-- | A 'Capture'-able segment corresponding to hackage v2's
-- @:something.:format@. The @:format@ is given and enforced statically.
type WithFormat :: Type -> Symbol -> Type
newtype WithFormat a b = WithFormat {unWithFormat :: a}

instance (FromHttpApiData a, KnownSymbol b) => FromHttpApiData (WithFormat a b) where
  parseUrlPiece t = do
    let ext = "." <> T.pack (symbolVal (Proxy @b))
    case T.isSuffixOf ext t of
      True -> parseUrlPiece $ T.dropEnd (T.length ext) t
      False -> Left $ "Non-matching format"

instance (ToHttpApiData a, KnownSymbol b) => ToHttpApiData (WithFormat a b) where
  toUrlPiece (WithFormat x) = toUrlPiece x <> "." <> T.pack (symbolVal (Proxy @b))

-- | A 'Capture'-able segment corresponding to hackage v2's
-- @:something.:format@. Unlike 'WithFormat', this combinator accepts any
-- format.
type WithAnyFormat :: Type -> Type
newtype WithAnyFormat a = WithAnyFormat {unWithAnyFormat :: a}


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


-- | Custom combinator for doing the same auth checks as Hackage v2.
type HackageAuth = AuthProtect "hackage-auth"
type instance AuthServerData HackageAuth = UserId


-- | Generate an 'AuthHandler' for 'HackageAuth' auth.
hackageAuthHandler :: RealmName -> Connection -> AuthHandler Request UserId
hackageAuthHandler realm conn = mkAuthHandler $ \req -> Handler $ ExceptT $ do
  eauth <- runExceptT $ checkAuthenticated hackageRealm conn req
  case eauth of
    Left err -> fmap Left $ authErrorResponse realm err
    Right a -> pure $ pure a


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


type instance TemplateFiles NotYetPorted = '[]
type instance TemplateFiles PermanentRedirect = '[]


--------------------------------------------------------------------------------

-- | Endpoints whose content can be negotiated by giving a file extension. Eg,
-- you can request @text/json@ by appending @.json@ to the URI. For example:
--
-- @NegotiableContent :> "resource" :> Get '[HTML, JSON] ...@
--
-- will match routes @/resource@, @/resource.html@, and @/resource.json@. It
-- will use standard content negotiation for the @/resource@ route.
data NegotiableContent
type instance TemplateFiles NegotiableContent = '[]

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
