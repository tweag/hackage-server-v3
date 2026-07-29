{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Hasql.Connection.Setting qualified as DB
import Hasql.Connection.Setting.Connection qualified as DB
import Control.Monad (replicateM_)
import Control.Monad.Except
import Data.Aeson (Value (..), decode, fromJSON, toJSON, Result (..))
import Data.Aeson.KeyMap qualified as KM
import Data.BlobStorage qualified as Blob
import Data.ByteString.Lazy (ByteString)
import Data.Kind (Constraint, Type)
import Data.Pool
import Data.Proxy (Proxy(..))
import Data.String (fromString)
import Data.Time (UTCTime (..))
import Data.Vector qualified as V
import Distribution.Package (PackageName, PackageIdentifier(..))
import Distribution.Version (Version)
import Hackage.API.PackageDb
import Hackage.API.Type
import Hackage.Main
import Hackage.Objects
import Hackage.Schemas.Packages
import Hackage.ServerM
import Hackage.Utils
import Network.HTTP.Request qualified as Req
import Rel8 (Serializable, FromExprs, Query, Expr, countRows, offset, limit, (==.), each, where_)
import Servant.API
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
hackageBase = "http://hackage.haskell.org"


testOptions :: Options
testOptions = Options
  { optDb = DB.string "postgresql://sandy@/sandy"
  , optBlobStore = "../hackage-server/state/blobs"
  , optConnections = 100
  , optPort = 8000
  }


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
    link <- liftIO $ withConn (pure $ DB.connection $ optDb testOptions) $ \conn -> fillLink conn $ fieldLink field
    let uri = show (linkURI link)
    annotate uri $ do
      Req.Response _ _ bsv2 <- liftIO $ Req.get @ByteString $ hackageBase </> uri
      get (fromString uri) `shouldRespondWith` ResponseMatcher 200 [] (MatchBody $ \_ bsv3 ->
        case (,) <$> decode @Value bsv2 <*> decode @Value bsv3 of
          Just (v2, v3) ->
            case roughlyEq v2 v3 of
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
      blobStore <- Blob.open "blobs"
      runServerM
        (Proxy @(NamedRoutes PackageDbApi))
        EmptyContext
        (ServerCtx pool blobStore)
        packageDbServer
      ) $ do
    xit "htmlMirrorUploadTime" $ verify pkgdb_api_uploadTime
    it "htmlTarballs" $ verify pkgdb_api_metadata
    xit "htmlTarballs" $ verify pkgdb_api_revisions


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


-- | Compare two 'Value's for equality, truncating any 'UTCTime's down to the
-- nearest second. Hackage v2 responds with picosecond precision, but we've
-- imported v3 data from the index tarball which has only second precision ---
-- thus, this function quotients by that difference.
roughlyEq :: Value -> Value -> Bool
roughlyEq (Object a) (Object b) =
  KM.keys a == KM.keys b &&
    and (KM.intersectionWith roughlyEq a b)
roughlyEq (Array a) (Array b) =
  V.length a == V.length b
    && and (V.zipWith roughlyEq a b)
roughlyEq a b = truncateTime a == truncateTime b


truncateTime :: Value -> Value
truncateTime v
  | Success (UTCTime day dt) <- fromJSON @UTCTime v
  = toJSON $ UTCTime day $ fromIntegral $ floor @_ @Int dt
  | otherwise = v


main :: IO ()
main = hspec spec

