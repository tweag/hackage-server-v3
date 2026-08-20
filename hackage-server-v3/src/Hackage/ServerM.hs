{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-orphans       #-}

module Hackage.ServerM where

import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.BlobStorage (BlobStorage)
import Data.Markdown
import Data.Pool
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import Distribution.Pretty qualified as Pretty
import Distribution.Types.PackageId (PackageIdentifier(..))
import GHC.Exts (IsList(..))
import Hackage.Objects ()
import Hasql.Connection (Connection)
import Servant
import Servant.EDE
import Text.EDE.Filters
import Text.EDE.Internal.Filters (qlist1)


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


runServerM :: ServerCtx -> ServerM a -> IO (Either ServerError a)
runServerM ctx = runExceptT . flip runReaderT ctx . unServerM


serverMToWai
    :: forall api ctx
     . ( LoadedTemplates => HasServer api ctx
       , ServerContext ctx
       , TemplateFiles Trivial api
       )
    => Proxy api
    -> Context ctx
    -> ServerCtx
    -> ServerT api ServerM
    -> IO Application
serverMToWai api ctx serverCtx server = either (fail . show) pure =<< do
  unsafeLoadTemplates api filters "templates" ()
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
  , "renderMarkdown" @: (renderMarkdown "" . encodeUtf8)
  , "renderMarkdownRel" @: (renderMarkdownRel "" . encodeUtf8)
  , "timePretty" @: (T.pack . formatTime @UTCTime defaultTimeLocale "%a %b %d %T UTC %Y")
  , qlist1 "cat" (<>) (<>)
  ]

toPackageUrl :: PackageIdentifier -> Text
toPackageUrl pkg = T.pack $ "package/" <> Pretty.prettyShow pkg

instance Unquote UTCTime

