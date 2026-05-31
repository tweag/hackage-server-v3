{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Servant.HackageCombinators where

import Data.Functor ((<&>))
import Control.Monad.IO.Class
import Servant.Server
import Data.Proxy (Proxy (..))
import Servant.API
import Data.Kind (Type)
import GHC.TypeLits
import Data.Text qualified as T
import Servant.Server.Internal.Router
import Servant.Server.Internal.Delayed
import Control.Monad.Trans.Resource (runResourceT)
import Network.Wai
import Network.HTTP.Types.Status (permanentRedirect308)
import Network.HTTP.Types.Header (hLocation)


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

