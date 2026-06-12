module Hackage.ServerM where

import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Hasql.Connection (Connection)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Except
import Servant
import Data.Pool


data ServerCtx = ServerCtx
  { serverPool :: Pool Connection
  }


newtype ServerM a = ServerM
  { unServerM :: ReaderT ServerCtx (ExceptT ServerError IO) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadError ServerError
    , MonadReader ServerCtx
    , MonadIO
    )


withConnection :: (Connection -> ServerM a) -> ServerM a
withConnection k =
  ServerM $ ReaderT $ \ctx ->
    ExceptT $
      withResource (serverPool ctx) $
        runExceptT . flip runReaderT ctx . unServerM . k


runServerM
    :: forall api ctx
     . (HasServer api ctx, ServerContext ctx)
    => Proxy api
    -> Context ctx
    -> ServerCtx
    -> ServerT api ServerM
    -> Application
runServerM api ctx serverCtx
  = serveWithContext api ctx
  . hoistServerWithContext
      api
      (Proxy @ctx)
      (Handler . flip runReaderT serverCtx . unServerM)

