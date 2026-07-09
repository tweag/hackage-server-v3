{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module DBArbitrary where

import Control.Monad (replicateM_)
import Control.Monad.Except
import Data.BlobStorage qualified as Blob
import Data.Kind (Constraint, Type)
import Data.Pool
import Data.Proxy (Proxy(..))
import Data.String (fromString)
import Distribution.Package (PackageName, PackageIdentifier(..))
import Distribution.Version (Version)
import Hackage.API.PackagesHTML
import Hackage.Objects
import Hackage.Schemas.Packages
import Hackage.ServerM
import Hackage.Utils
import Network.HTTP.Request qualified as Req
import Rel8 hiding (with)
import Servant.API
import Servant.Links (fieldLink, linkURI)
import Servant.Server
import System.FilePath ((</>))
import System.Random (randomRIO, randomIO)
import Test.Hspec
import Test.Hspec.Wai
import TestAPI hiding (main)


type DBArbitrary :: Type -> Constraint
class DBArbitrary a where
  type DBArbitraryExpr a :: Type
  type DBArbitraryExpr a = Expr a

  queryArbitrary :: Query (DBArbitraryExpr a)

  fromIntermediary :: FromExprs (DBArbitraryExpr a) -> IO a
  default fromIntermediary :: DBArbitraryExpr a ~ Expr a => FromExprs (DBArbitraryExpr a) -> IO a
  fromIntermediary = pure


instance DBArbitrary PackageName where
  queryArbitrary = do
    pkgs <- each packageNameSchema
    pure $ packageName pkgs


instance DBArbitrary PackageIdentifier where
  type DBArbitraryExpr PackageIdentifier = (Expr PackageName, Expr Version)

  queryArbitrary = do
    pkginfo <- each pkgInfoSchema
    pkg <- each packageNameSchema
    where_ $ packageNameId pkg ==. pkgId pkginfo
    pure (packageName pkg, packageVersion pkginfo)

  fromIntermediary = pure . uncurry PackageIdentifier


instance DBArbitrary PackageLocator where
  type DBArbitraryExpr PackageLocator = (Expr PackageName, Expr Version)
  queryArbitrary = queryArbitrary @PackageIdentifier
  fromIntermediary a = do
    pid <- fromIntermediary @PackageIdentifier a
    randomIO >>= pure . \case
      False -> Latest $ pkgName pid
      True -> Specific pid


randomQueryArbitrary :: forall a. DBArbitrary a => Word -> Query (DBArbitraryExpr a)
randomQueryArbitrary off = limit 1 $ offset off $ queryArbitrary @a

hackageBase :: String
hackageBase = "http://hackage.haskell.org"

verify
  :: ( FillLink (MkLink endpoint Link)
     , HasLink endpoint
     , Filled (MkLink endpoint Link) ~ Link
     , IsElem endpoint (ToServantApi routes)
     , GenericServant routes AsApi
     )
  => (routes AsApi -> endpoint)
  -> WaiSession st ()
verify field = do
  replicateM_ 100 $ do
    link <- liftIO $ mkConn $ \conn -> fillLink conn $ fieldLink field
    let uri = show (linkURI link)
    annotate uri $ do
      Req.Response _ _ body <- liftIO $ Req.get @String $ hackageBase </> uri
      get (fromString uri) `shouldRespondWith` fromString body

spec :: Spec
spec =
  with (do
      pool <- newPool connPool
      blobStore <- Blob.open "blobs"
      runServerM
        (Proxy @(NamedRoutes PackagesHtmlAPI))
        EmptyContext
        (ServerCtx pool blobStore)
        packagesHtmlServer
      ) $ do
    xit "htmlMirrorUploadTime" $ verify htmlMirrorUploadTime
    it "htmlTarballs" $ verify htmlPackageMetadata
    xit "htmlTarballs" $ verify htmlPackageRevisions


dbArbitrary
  :: forall a
   . ( DBArbitrary a
     , Serializable (DBArbitraryExpr a) (FromExprs (DBArbitraryExpr a))
     )
  => Connection
  -> IO (Either SessionError a)
dbArbitrary conn = runExceptT $ do
  size <- ExceptT $ doSelect1 (countRows $ queryArbitrary @a) conn
  off <- randomRIO (0, size - 1)

  ExceptT $ doSelect1 (randomQueryArbitrary @a $ fromIntegral off) conn >>= \case
    Left e -> pure $ Left e
    Right a -> fmap Right $ fromIntermediary a


main :: IO ()
main = do
  link  <- mkConn $ \conn -> fillLink conn $ fieldLink htmlPackageDeps
  let uri = "http://hackage.haskell.org/" <> show (linkURI link)
  putStrLn uri
  Req.Response _ _ body <- Req.get @String uri
  putStrLn body


class FillLink a where
  type Filled a
  fillLink :: Connection -> a -> IO (Filled a)

instance (DBArbitrary a, Serializable (DBArbitraryExpr a) (FromExprs (DBArbitraryExpr a)), FillLink b) => FillLink (a -> b) where
  type Filled (a -> b) = Filled b
  fillLink conn f = do
    Right a <- dbArbitrary @a conn
    fillLink @b conn $ f a

instance FillLink Link where
  type Filled Link = Link
  fillLink _ l = pure l

