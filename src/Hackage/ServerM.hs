{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Hackage.ServerM where

import GHC.Exts (IsList(..))
import Distribution.Pretty qualified as Pretty
import Distribution.Types.PackageId (PackageIdentifier(..))
import Data.Text (Text)
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Hasql.Connection (Connection)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Except
import Servant
import Text.EDE.Internal.Filters (qlist1)
import Data.Pool
import Servant.EDE
import Data.BlobStorage (BlobStorage)
import Data.Text qualified as T
import Text.EDE.Filters
import Hackage.Objects ()


data ServerCtx = ServerCtx
  { serverPool :: Pool Connection
  , serverBlobStore :: BlobStorage
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
     . ( LoadedTemplates => HasServer api ctx
       , ServerContext ctx
       , TemplateFiles api
       )
    => Proxy api
    -> Context ctx
    -> ServerCtx
    -> ServerT api ServerM
    -> IO Application
runServerM api ctx serverCtx server = either (fail . show) pure =<< do
  unsafeLoadTemplates api filters "templates"
    $ pure
    $ serveWithContext api ctx
    $ hoistServerWithContext
        api
        (Proxy @ctx)
        (Handler . flip runReaderT serverCtx . unServerM)
        server

filters :: (IsList l, Item l ~ (Text, Term)) => l
filters =
  [ "toPackageUrl" @: toPackageUrl
  , "packageName" @: pkgName
  , "packagePretty" @: (T.pack . Pretty.prettyShow @PackageIdentifier)
  , "intercalate" @: \x y -> T.intercalate y x
  , qlist1 "cat" (<>) (<>)
  ]

toPackageUrl :: PackageIdentifier -> Text
toPackageUrl pkg = T.pack $ "package/" <> Pretty.prettyShow pkg

