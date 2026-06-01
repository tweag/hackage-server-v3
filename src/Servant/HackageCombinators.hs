{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Servant.HackageCombinators where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import GHC.TypeLits
import Hackage.Types
import Hackage.Utils (Connection)
import Network.HTTP.Types.Header (hLocation)
import Network.HTTP.Types.Status (permanentRedirect308)
import Network.Wai
import Servant.API
import Servant.HackageAuth
import Servant.Server
import Servant.Server.Experimental.Auth
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.Router


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

