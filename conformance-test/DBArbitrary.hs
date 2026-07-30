{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Main where

import Control.Monad (replicateM_)
import Control.Monad.Except
import Data.Aeson (Value (..), decode)
import Data.BlobStorage qualified as Blob
import Data.ByteString.Lazy (ByteString)
import Data.Kind (Constraint, Type)
import Data.Pool
import Data.Proxy (Proxy(..))
import Data.String (fromString)
import Distribution.Package (PackageName, PackageIdentifier(..))
import Distribution.Version (Version)
import Hackage.API.PackageDb
import Hackage.API.Type
import Hackage.Main
import Hackage.Objects
import Hackage.Schemas.Packages
import Hackage.ServerM
import Hackage.Utils
import Hasql.Connection.Setting qualified as DB
import Hasql.Connection.Setting.Connection qualified as DB
import Network.HTTP.Request qualified as Req
import Rel8 (Serializable, FromExprs, Query, Expr, countRows, offset, limit, (==.), each, where_, present)
import Servant.API
import Servant.HackageCombinators.NegotiableContent
import Servant.HackageCombinators.UserDomain (UserDomain(..))
import Servant.Links (fieldLink, linkURI)
import Servant.Server
import System.FilePath ((</>))
import System.Random (randomRIO, randomIO)
import Test.Hspec
import Test.Hspec.Wai


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
    -- Ensure we actually have a tarball for this pkgid.
    present $ do
      tar <- each packageTarballRevisionsSchema
      where_ $ tarballPkgId tar ==. pkgInfoId pkginfo
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

instance DBArbitrary String where
  queryArbitrary = pure "json"


randomQueryArbitrary :: forall a. DBArbitrary a => Word -> Query (DBArbitraryExpr a)
randomQueryArbitrary off = limit 1 $ offset off $ queryArbitrary @a

hackageBase :: String
hackageBase = "http://localhost:8080"


testOptions :: Options
testOptions = Options
  { optDb = DB.string "postgresql://sandy@/sandy"
  , optBlobStore = "../hackage-server/state/blobs"
  , optConnections = 100
  , optPort = 8000
  , optUserDomain = ""
  }


verify
  :: (Connection -> IO Link)
  -> WaiSession st ()
verify mklink = do
  replicateM_ 100 $ do
    link <- liftIO $ withConn (pure $ DB.connection $ optDb testOptions) mklink
    let uri = show (linkURI link)
    annotate uri $ do
      Req.Response _code _ bsv2 <- liftIO $ Req.get @ByteString $ hackageBase </> uri
      get (fromString uri) `shouldRespondWith` ResponseMatcher 200 [] (MatchBody $ \_ bsv3 ->
        -- case code == 404 of
        --   True -> Nothing
        --   False ->
            case (,) <$> decode @Value bsv2 <*> decode @Value bsv3 of
              Just (v2, v3) ->
                case v2 == v3 of
                  True -> Nothing
                  False -> Just $ unlines
                    [ show v2
                    , show v3
                    ]
              Nothing ->
                case bsv2 == bsv3 of
                  True -> Nothing
                  False -> Just $ unlines
                    [ show bsv2
                    , show bsv3
                    ]
        )

spec :: Spec
spec =
  with (do
      pool <- newPool $ connPool testOptions
      blobStore <- Blob.open "../hackage-server/state/blobs"
      runServerM
        (Proxy @(NamedRoutes PackageDbApi))
        (UserDomain (optUserDomain testOptions) :. EmptyContext)
        (ServerCtx pool blobStore)
        packageDbServer
      ) $ do
    it "revisions" $ verify $ \conn -> do
      Right a <- dbArbitrary conn
      pure $ fieldLink pkgdb_api_revisions_redirect (Just $ NegotiatedContent "json") a
    it "tarball" $ verify $ \conn -> do
      Right a <- dbArbitrary conn
      pure $ fieldLink pkgdb_api_tarball (Specific a) a
    it "uploader" $ verify $ \conn -> do
      Right a <- dbArbitrary conn
      pure $ fieldLink pkgdb_api_uploader a
    it "upload time" $ verify $ \conn -> do
      Right a <- dbArbitrary conn
      pure $ fieldLink pkgdb_api_uploadTime a
    it "versions" $ verify $ \conn -> do
      Right a <- dbArbitrary conn
      pure $ fieldLink pkgdb_api_versions a
    it "metadata" $ verify $ \conn -> do
      Right a <- dbArbitrary conn
      pure $ fieldLink pkgdb_api_metadata a
    it "cabalFile" $ verify $ \conn -> do
      Right a <- dbArbitrary conn
      pure $ fieldLink pkgdb_api_cabalFile a a
    it "preferredVersions" $ verify $ \conn -> do
      Right a <- dbArbitrary conn
      pure $ fieldLink pkgdb_api_preferredVersions (Just $ NegotiatedContent "json") a


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
main = hspec spec

