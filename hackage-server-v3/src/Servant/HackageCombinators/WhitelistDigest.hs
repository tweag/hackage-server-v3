{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.HackageCombinators.WhitelistDigest where

import Data.IORef
import Data.Functor ((<&>))
import Data.Proxy
import Data.Set qualified as S
import Data.Set (Set)
import qualified Crypto.Hash as Crypto
import Control.Monad.IO.Class
import Network.Wai (responseToStream, responseLBS, Response)
import Servant.Server hiding (respond)
import Servant.Server.Internal.Router
import Servant.API
import Servant.EDE
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString (toStrict)

import Servant.Server.Internal.RouteResult


-- | Servant combinator which behaves like @Get contentType a@, except that it
-- ensures the final response has a known SHA256 digest. This can be useful for
-- serving untrusted user files by checking that they haven't been modified
-- from a known-good state.
data WhitelistDigest contentType a


-- | Handler for the 'WhitelistDigest' combinator.
data WithWhitelistDigest a = WithWhitelistDigest
  { digests :: Set (Crypto.Digest Crypto.SHA256)
            -- ^ Acceptable digests for the final response
  , subhandler :: a
  }
  deriving stock (Functor, Foldable, Traversable)


instance (rewrite ~ Get contentType a, HasServer rewrite context) => HasServer (WhitelistDigest contentType a) context where
  type ServerT (WhitelistDigest contentType a) m = WithWhitelistDigest (m a)
  hoistServerWithContext _ a b = fmap $ hoistServerWithContext (Proxy @rewrite) a b
  route _ ctx app = RawRouter $ \env req respond -> do
    -- This is a bit of a mess, because we're doing things that evidently
    -- servant doesn't want us to. We want to exfiltrate the 'digests' from
    -- @app@, but there's no straightforward way to do it. So the idea is to
    -- make this 'IORef', wrap the handler to fill it, and then read the data
    -- out when we have a response.
    digestsRef <- newIORef mempty
    runRouterEnv
      (notFoundErrorFormatter defaultErrorFormatters)
      (route (Proxy @rewrite) ctx $ app <&> \(WithWhitelistDigest digests handler) -> do
        liftIO $ writeIORef digestsRef digests
        handler
      ) env req $ \rresp -> do
          -- Now we have our hands on the digests and the response...
          digests <- readIORef digestsRef
          case rresp of
            -- If the subhandler succeeded, then we take the digest of the
            -- response body and check it against our 'digests'.
            Route resp -> do
              (resp', digest) <- responseBodyDigest resp
              case S.member digest digests of
                True -> respond $ pure resp'
                False ->
                  respond $ FailFatal $ err403
                    { errReasonPhrase = "Whitelisted digest is not correct"
                    , errBody = "Whitelisted digests are listed in the hackage-server source code."
                    }
            -- Otherwise, routing failed, and we just forward the errors.
            Fail x -> respond $ Fail x
            FailFatal x -> respond $ FailFatal x


-- | Compute the SHA256 digest of the response body. Note that this returns
-- a new 'Response', because we might have done a bunch of IO to read the body,
-- and there's no reason to re-do all of that work.
--
-- You probably don't want to use this on streaming responses!
responseBodyDigest :: Response -> IO (Response, Crypto.Digest Crypto.SHA256)
responseBodyDigest resp = do
  bodyRef <- newIORef mempty
  let (status, headers, streamBody) = responseToStream resp
  streamBody $ \streamingBody ->
    streamingBody (\chunk -> modifyIORef bodyRef (<> chunk)) $ pure ()
  body <- fmap toLazyByteString $ readIORef bodyRef
  pure
    ( responseLBS status headers body
    , Crypto.hashWith Crypto.SHA256 $ toStrict body
    )

instance TemplateFiles (Get ct a) => TemplateFiles (WhitelistDigest ct a) where
  templateFiles _ = templateFiles $ Proxy @(Get ct a)

