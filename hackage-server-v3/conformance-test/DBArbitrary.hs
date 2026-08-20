{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}

module Main where

import Control.Monad (replicateM_)
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT(..))
import Data.Aeson (Value (..), decode)
import Data.BlobStorage qualified as Blob
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS8
import Data.Char (isSpace)
import Data.List (sort)
import Data.Pool
import Data.Proxy (Proxy(..))
import Data.String (fromString)
import Hackage.API.PackageDb
import Hackage.API.Query (getLocator)
import Hackage.API.Type
import Hackage.Main hiding (main)
import Hackage.Objects
import Hackage.Schemas.Packages
import Hackage.ServerM
import Hackage.Types hiding (Tag)
import Hackage.Utils
import Hasql.Connection.Setting qualified as DB
import Hasql.Connection.Setting.Connection qualified as DB
import Network.HTTP.Request qualified as Req
import Rel8 (Serializable, FromExprs, Query, countRows, offset, limit, (==.), each, where_, present)
import Servant.API
import Servant.HackageCombinators.NegotiableContent
import Servant.HackageCombinators.UserDomain (UserDomain(..))
import Servant.Links (fieldLink, linkURI)
import Servant.Server
import System.FilePath ((</>))
import System.Random (randomRIO, randomIO)
import Test.Hspec
import Test.Hspec.Wai
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Text.HTML.TagSoup.Tree


-- | A monadic generator for random values drawn from the database.
newtype DBGen a = DBGen { unDBGen :: DatabaseM a }
  deriving newtype (Functor, Applicative, Monad)

instance MonadIO DBGen where
  liftIO = DBGen . DatabaseM . liftIO


-- | Run a generator against a connection.
sampleGen :: DBGen a -> Connection -> IO (Either SessionError a)
sampleGen g conn = runExceptT $ flip runReaderT conn $ unDatabaseM $ unDBGen g

pickRandom
  :: Serializable expr (FromExprs expr)
  => Query expr
  -> DBGen (FromExprs expr)
pickRandom q = DBGen $ do
  size <- doSelect1 (countRows q)
  off  <- DatabaseM $ liftIO $ randomRIO (0, max 0 $ size - 1)
  doSelect1 (limit 1 $ offset (fromIntegral off) q)


genPackageName :: DBGen PackageName
genPackageName = pickRandom $ do
  pkgs <- each packageNameSchema
  pure $ packageName pkgs


genPackageId :: DBGen PackageIdentifier
genPackageId = do
  (pn, v) <- pickRandom $ do
    pkginfo <- each pkgInfoSchema
    pkg     <- each packageNameSchema
    where_ $ packageNameId pkg ==. pkgId pkginfo
    -- Ensure we actually have a tarball for this pkgid.
    present $ do
      tar <- each packageTarballRevisionsSchema
      where_ $ tarballPkgId tar ==. pkgInfoId pkginfo
    pure (packageName pkg, packageVersion pkginfo)
  pure $ PackageIdentifier pn v


genMetadataRevIx :: PackageLocator -> DBGen MetadataRevIx
genMetadataRevIx loc = do
  pickRandom $ do
    pkg <- getLocator loc
    rev <- each metadataRevisionsSchema
    where_ $ metadataPkgId rev ==. pkgInfoId pkg
    pure $ metadataRevId rev


genPackageLocator :: DBGen PackageLocator
genPackageLocator = do
  pid <- genPackageId
  liftIO randomIO >>= \case
    False -> pure $ Latest  (pkgName pid)
    True  -> pure $ Specific pid


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


-- | Given two lists, find the tails that don't match.
unmatching :: Eq a => [a] -> [a] -> ([a], [a])
unmatching [] ys = ([], ys)
unmatching (x : xs) (y : ys)
  | x == y = unmatching xs ys
  | otherwise = (x : xs, y : ys)
unmatching (x : xs) [] = (x : xs, [])


verify
  :: DBGen Link
  -> WaiSession st ()
verify mklink = do
  replicateM_ 100 $ do
    Right link <-
      liftIO $ withConn (pure $ DB.connection $ optDb testOptions) $ sampleGen mklink
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
              Nothing -> do
                let tagsv2 = simplifyTags $ parseTags bsv2
                    tagsv3 = simplifyTags $ parseTags bsv3
                case unmatching tagsv2 tagsv3 of
                  ([], []) -> Nothing
                  (utagsv2, utagsv3) -> do
                    let (x, y) = unmatching (reverse utagsv2) (reverse utagsv3)
                    Just $ unlines
                      [ show $ reverse x
                      , show $ reverse y
                      ]
        )


trimBS :: ByteString -> ByteString
trimBS = BS8.dropWhile isSpace . BS8.dropWhileEnd isSpace

simplifyTags :: [Tag ByteString] -> [Tag ByteString]
simplifyTags
  = filter
      ( \case
        -- V2 has mangled html sometimes and often forgets its closing tags.
        TagClose{} -> False
        -- V2 is inconsistent about its doctypes
        TagOpen "!DOCTYPE" _ -> False
        -- V2 is inconsistent about its html attributes
        TagOpen "html" _ -> False
        -- V2 is inconsistent about its metas
        TagOpen "meta" _ -> False
        -- V2 is inconsistent about its links
        TagOpen "link" _ -> False
        -- V2 is inconsistent about its scripts
        TagOpen "script" _ -> False
        _ -> True
      )
  . fmap
      ( \case
        TagOpen a b -> TagOpen a (sort b)
        x -> x
      )
  . flattenTree
  . transformTree
      ( \case
          TagLeaf (TagText s) ->
            case trimBS s of
              "" -> mempty
              s' -> pure $ TagLeaf $ TagText $ BS8.filter (not . isSpace) s'
          TagBranch "style" _ _ -> mempty
          TagBranch "ul" (anyAttrLit ("id", "page-menu") -> True) _ -> mempty
          x -> pure x
      )
  . tagTree
  . canonicalizeTags


spec :: Spec
spec =
  with (do
      pool <- newPool $ connPool testOptions
      blobStore <- Blob.open "../../hackage-server/state/blobs"
      serverMToWai
        (Proxy @(NamedRoutes PackageDbApi))
        (UserDomain (optUserDomain testOptions) :. EmptyContext)
        (ServerCtx pool blobStore)
        packageDbServer
      ) $ do
    describe "non-html endpoints" $ do
      it "revisions" $ verify $ do
        a <- genPackageLocator
        pure $ fieldLink pkgdb_api_revisions_redirect (Just $ NegotiatedContent "json") a
      it "revision metadata" $ verify $ do
        a <- genPackageId
        rev <- genMetadataRevIx $ Specific a
        pure $ fieldLink pkgdb_api_revisionMetadata (Just $ NegotiatedContent "json") a rev
      it "revision cabal" $ verify $ do
        a <- genPackageId
        rev <- genMetadataRevIx $ Specific a
        pure $ fieldLink pkgdb_api_revisionCabal a rev
      it "tarball" $ verify $ do
        a <- genPackageId
        pure $ fieldLink pkgdb_api_tarball (Specific a) a
      it "uploader" $ verify $ do
        a <- genPackageLocator
        pure $ fieldLink pkgdb_api_uploader a
      it "upload time" $ verify $ do
        a <- genPackageLocator
        pure $ fieldLink pkgdb_api_uploadTime a
      it "versions" $ verify $ do
        a <- genPackageName
        pure $ fieldLink pkgdb_api_versions a
      it "metadata" $ verify $ do
        a <- genPackageId
        pure $ fieldLink pkgdb_api_metadata a
      it "cabalFile" $ verify $ do
        a <- genPackageLocator
        pure $ fieldLink pkgdb_api_cabalFile a $ packageLocName a
      it "preferredVersions" $ verify $ do
        a <- genPackageName
        pure $ fieldLink pkgdb_api_preferredVersions (Just $ NegotiatedContent "json") a
      it "deprecated" $ verify $ do
        a <- genPackageName
        pure $ fieldLink pkgdb_api_deprecated (Just $ NegotiatedContent "json") a
      it "all deprecated" $ verify $ do
        pure $ fieldLink pkgdb_api_allDeprecated $ Just $ NegotiatedContent "json"
      it "docs tarball" $ verify $ do
        a <- genPackageLocator
        pure $ fieldLink pkgdb_api_docsTarball a
      it "changelog" $ verify $ do
        a <- genPackageLocator
        pure $ fieldLink pkgdb_api_changelog (Just $ NegotiatedContent "txt") a

    describe "html endpoints" $ do
      it "revisions" $ verify $ do
        a <- genPackageLocator
        pure $ fieldLink pkgdb_api_revisions_redirect (Just $ NegotiatedContent "html") a
      it "distro monitor" $ verify $ do
        a <- genPackageName
        pure $ fieldLink pkgdb_api_distroMonitor Nothing a
      it "dependencies" $ verify $ do
        a <- genPackageLocator
        pure $ fieldLink pkgdb_api_dependencies a
      it "preferredVersions" $ verify $ do
        a <- genPackageName
        pure $ fieldLink pkgdb_api_preferredVersions (Just $ NegotiatedContent "html") a
      it "deprecated" $ verify $ do
        a <- genPackageName
        pure $ fieldLink pkgdb_api_deprecated (Just $ NegotiatedContent "html") a
      it "all deprecated" $ verify $ do
        pure $ fieldLink pkgdb_api_allDeprecated $ Just $ NegotiatedContent "html"
      it "changelog" $ verify $ do
        a <- genPackageLocator
        pure $ fieldLink pkgdb_api_changelog (Just $ NegotiatedContent "html") a


main :: IO ()
main = hspec spec

