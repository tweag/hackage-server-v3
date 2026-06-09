{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.HackageCombinators.CaptureExt where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Servant.API
import Servant.Server hiding (respond)
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.Router
import Servant.Server.Internal (delayedFail, mkContextWithErrorFormatter, MkContextWithErrorFormatter)
import Data.Typeable (Typeable, typeRep)
import Servant.Server.Internal.DelayedIO (withRequest)


-- | A 'Capture'-able segment corresponding to hackage v2's
-- @:something.:format@. The @:format@ is given and enforced statically.
type CaptureExt :: Symbol -> Type -> Symbol -> Type
data CaptureExt hint a ext

instance ( Typeable a
         , HasServer api ctx
         , KnownSymbol hint
         , KnownSymbol ext
         , FromHttpApiData a
         , HasContextEntry (MkContextWithErrorFormatter ctx) ErrorFormatters
         ) => HasServer (CaptureExt hint a ext :> api) ctx where
  type ServerT (CaptureExt hint a ext :> api) m = a -> ServerT api m
  hoistServerWithContext _ b c k = hoistServerWithContext (Proxy @api) b c . k
  route _ context d =
    CaptureRouter [hint] $
        route (Proxy @api) context $ addCapture d $ \txt -> withRequest $ \request -> do
          let ext = T.pack $ "." <> symbolVal (Proxy @ext)
          case T.isSuffixOf ext txt of
            True ->
              case parseUrlPiece (T.dropEnd (T.length ext) txt) of
                Right val -> pure val
                Left e  -> delayedFail $ formatError rep request $ T.unpack e
            False -> delayedFail err404
    where
      rep = typeRep (Proxy :: Proxy Capture')
      formatError = urlParseErrorFormatter $ getContextEntry (mkContextWithErrorFormatter context)
      hint = CaptureHint (T.pack $ symbolVal $ Proxy @hint) (typeRep (Proxy :: Proxy a))

