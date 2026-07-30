{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans   #-}

module Hackage.API.PackageDb
  ( packageDbServer
  ) where

import Hackage.API.Type
import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Control.Monad (unless, guard)
import Control.Monad.Except (throwError)
import Control.Monad.Reader
import Data.Aeson hiding (Result(..))
import Data.BlobStorage qualified as Blob
import Data.Bool
import Data.ByteString (StrictByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Functor
import Data.Functor.Contravariant
import Data.Int (Int64)
import Data.List qualified as List
import Data.Map qualified as M
import Data.Proxy (Proxy(..))
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Arbitrary ()
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Time (UTCTime)
import Data.Trie
import Distribution.License (licenseToSPDX)
import Distribution.PackageDescription.Parsec qualified as PkgDescr
import Distribution.Pretty qualified as Pretty
import Distribution.Types.Dependency as Cabal
import Distribution.Types.GenericPackageDescription qualified as PkgDescr
import Distribution.Types.PackageDescription qualified as PkgDescr
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.VersionRange (anyVersion)
import Distribution.Utils.MD5 (md5, showMD5)
import Distribution.Utils.ShortText (fromShortText)
import Hackage.API.Query
import Hackage.Objects
import Hackage.Schemas.Packages
import Hackage.Schemas.Users
import Hackage.ServerM
import Hackage.Types
import Hackage.Utils
import Network.HTTP.Types.Header (hLocation)
import Rel8 hiding (Lift, bool)
import Servant.API
import Servant.EDE
import Servant.HackageCombinators.DynamicGet
import Servant.HackageCombinators.NegotiableContent
import Servant.Links
import Servant.Server (err303, err404, err500, ServerError(..))
import Servant.Server.Generic (AsServerT)
import System.IO


packageDbServer :: PackageDbApi (AsServerT ServerM)
packageDbServer = PackageDbApi
  { pkgdb_api_versions = packageVersions
  , pkgdb_api_cabalFile = packageCabalFile
  , pkgdb_api_metadata = packageMetadata
  , pkgdb_api_preferredVersions = packagePreferredVersions
  , pkgdb_api_uploader = packageUploader
  , pkgdb_api_uploadTime = packageUploadTime
  , pkgdb_api_tarball = packageTarball
  , pkgdb_api_distroMonitor = packageDistroMonitor
  , pkgdb_api_dependencies = packageDependencies
  , pkgdb_api_revisions = packageRevisions
  , pkgdb_api_tarballContent = packageTarballContent
  }


--------------------------------------------------------------------------------
-- /package/:packagename.json

packageVersions :: PackageName -> ServerM PackageVersions
packageVersions pname = do
  versions <- liftDB $ doSelect $ do
    pkgv <- getAllVersions $ lit pname
    pure (packageVersion pkgv, pkgInfoDeprecated pkgv)
  pure $ PackageVersions $ M.fromList $ fmap (fmap $ bool Normal Deprecated) versions


--------------------------------------------------------------------------------
-- /package/:packageid.json

packageMetadata :: PackageId -> ServerM PackageBasicDescriptionDTO
packageMetadata pid = do
  (rev, user) <- liftDB $ doSelect1 $ do
    rev <- getLatestRev $ Specific pid
    user <- each usersSchema
    where_ $ userId user ==. metadataUploader rev
    pure (rev, userName user)


  let parseResult = PkgDescr.parseGenericPackageDescription $ metadataCabalFile rev
  case PkgDescr.runParseResult parseResult of
    (_, Right pkg) -> do
      let pkgd = PkgDescr.packageDescription pkg
      pure $ PackageBasicDescriptionDTO
        { license = either id licenseToSPDX $ PkgDescr.licenseRaw pkgd
        , copyright = T.pack . fromShortText $ PkgDescr.copyright pkgd
        , synopsis = T.pack . fromShortText $ PkgDescr.synopsis pkgd
        , description = T.pack . fromShortText $ PkgDescr.description pkgd
        , homepage = T.pack . fromShortText $ PkgDescr.homepage pkgd
        , author = T.pack . fromShortText $ PkgDescr.author pkgd
        , metadata_revision = metadataRevId rev
        , uploaded_at = metadataTime rev
        , uploader = user
        }
    -- TODO(sandy): do something with the warnings?
    _ -> throwError $ err500


--------------------------------------------------------------------------------
-- /package/:package/:package.cabal

packageCabalFile :: PackageName -> PackageName -> ServerM StrictByteString
packageCabalFile pname1 pname2 = do
  -- For legacy reasons, this path requires both package names to be the same
  unless (pname1 == pname2) $ throwError err404
  liftDB $ doSelect1 $ do
    rev <- getLatestRev $ Latest pname1
    pure $ metadataCabalFile rev


--------------------------------------------------------------------------------
-- /package/:package/preferred

instance ToObject PreferredVersions where
  toObject (PreferredVersions vs) =
      [ "versions" .= M.mapKeys Pretty.prettyShow vs
      ]

instance HasTemplate HTML PreferredVersions where
  templateFor _ _ = "packages/preferred.html"


packagePreferredVersions
    :: Maybe NegotiatedContent
    -> PackageName
    -> ServerM (WithPackageName PreferredVersions)
packagePreferredVersions _ pname = do
  versions <- liftDB $ doSelect $ do
    pkgv <- getAllVersions $ lit pname
    pure (packageVersion pkgv, pkgInfoDeprecated pkgv)
  pure $ WithPackageName pname $ PreferredVersions $ M.fromList $ fmap (fmap $ bool Normal Deprecated) versions


--------------------------------------------------------------------------------
-- /package/:package/uploader

packageUploader :: PackageLocator -> ServerM UserName
packageUploader pname =
  liftDB $ doSelect1 $ do
    pkgv <- onlyLatestRev $ getAllRevs pname
    u <- each usersSchema
    where_ $ metadataUploader pkgv ==. userId u
    pure $ userName u


--------------------------------------------------------------------------------
-- /package/:package/upload-time

packageUploadTime :: PackageLocator -> ServerM UTCTime
packageUploadTime pname =
  liftDB $ doSelect1 $ do
    pkgv <- onlyEarliestRev $ getAllRevs pname
    pure $ metadataTime pkgv


--------------------------------------------------------------------------------
-- /package/:package/:tarball.tar.gz

packageTarball
    :: PackageLocator
    -> PackageIdentifier
    -> ServerM BSL.ByteString
packageTarball epname tarball = do
  -- We want to ensure that the tarball name lines up with the package we were
  -- given. If we have only a 'PackageName', then ensure it's the same package
  -- name as on the 'PackageIdentifier'.
  --
  -- If we have a full package identifier, then make sure they agree!
  case epname of
    Latest pname | pname == pkgName tarball -> pure ()
    Specific pid | pid == tarball -> pure ()
    _ -> throwError err404

  mblob <-
    liftDB $ doSelect1 $ optional $ fmap tarballBlobGz $ getLatestTarball $ Specific tarball
  case mblob of
    Just blob -> do
      store <- asks serverBlobStore
      liftIO $ Blob.get store blob
    Nothing -> throwError err404


--------------------------------------------------------------------------------
-- /package/:package/distro-monitor[.html]

instance ToObject AllTarballs where
  toObject (AllTarballs tbs) =
    [ "versions" .= fmap Pretty.prettyShow tbs
    ]

instance HasTemplate HTML AllTarballs where
  templateFor _ _ = "packages/distro-monitor.html"


packageDistroMonitor
    :: Maybe NegotiatedContent
    -> PackageName
    -> ServerM (WithPackageName AllTarballs)
packageDistroMonitor _ pname = do
  fmap (WithPackageName pname . AllTarballs . fmap (uncurry PackageIdentifier)) $ liftDB $ doSelect $ orderBy (snd >$< asc) $ do
    pkg <- each packageNameSchema
    where_ $ packageName pkg ==. lit pname
    pkgv <- each pkgInfoSchema
    where_ $ pkgId pkgv ==. packageNameId pkg
    pure (packageName pkg, packageVersion pkgv)


--------------------------------------------------------------------------------
-- /package/:package/dependencies

instance HasTemplate HTML Dependencies where
  templateFor _ _ = "packages/dependencies.html"

instance ToObject Dependencies where
  toObject (Dependencies b c) =
    [ "isCandidate" .= b
    , "dependencies" .= object (do
        (Dependency pkg vers libs) <- c
        pure $ fromString (Pretty.prettyShow $ Dependency pkg anyVersion libs) .= Pretty.prettyShow vers
        )
    ]


packageDependencies :: PackageLocator -> ServerM (WithPackage Dependencies)
packageDependencies pname = do
  rev <- liftDB $ doSelect1 $ do
    pkgv <- onlyLatestRev $ getAllRevs pname
    pure pkgv

  let parseResult = PkgDescr.parseGenericPackageDescription $ metadataCabalFile rev
  case PkgDescr.runParseResult parseResult of
    (_, Right pkg) -> do
      let pkgd = PkgDescr.packageDescription pkg
      pure $ WithPackage (PkgDescr.package pkgd) $ Dependencies False $ PkgDescr.allBuildDepends pkgd
    _ -> throwError $ err500


--------------------------------------------------------------------------------
-- /package/:package/revisions

instance HasTemplate HTML Revisions where
  templateFor _ _ = "packages/revisions.html"

instance ToObject Revisions where
  toObject (Revisions revs) =
    [ "revisions" .= revs
    ]


packageRevisions :: Maybe NegotiatedContent -> PackageLocator -> ServerM (WithPackage Revisions)
packageRevisions _ loc = do
  (name, version) <- liftDB $ doSelect1 $ locatorToPackageId loc
  revs <- liftDB $ doSelect $ do
    rev <- getAllRevs loc
    u <- each usersSchema
    where_ $ userId u ==. metadataUploader rev
    pure (rev, userName u)
  pure $ WithPackage (PackageIdentifier name version) $ Revisions $ revs <&> \(rev, user) ->
    Revision
      { number = metadataRevId rev
      , sha256 = T.pack $ showMD5 $ md5 $ metadataCabalFile rev
      , time = metadataTime rev
      , user = user
      }


--------------------------------------------------------------------------------
-- /package/:package/src/...

instance HasTemplate HTML DirectoryListing where
  templateFor _ _ = "packages/list-dir.html"

-- | We use 'ToObject' to generate EDE bindings, which in turn is used to
-- render HTML. However, EDE doesn't support recursion, but our trie is
-- arbitrarily recursive. Rather than serializing the trie itself, we instead
-- flatten it into a series of stack instructions which EDE can happily loop
-- over. It's a bit janky but it works.
instance ToObject DirectoryListing where
  toObject (DirectoryListing t) =
    [ "trie_cmds" .= flattenTrie t
    ]

packageTarballContent
    :: PackageLocator
    -> [Text]
    -> ServerM (OneOf '[ '(PlainText, Text)
                       , '(HTML, DirectoryListing)
                       ])
packageTarballContent loc ps = do
  -- Since all the paths in the package tarballs are prefixed by their pretty
  -- packageid, we must first resolve the locator.
  (pname, pid) <- liftDB $ doSelect1 $ locatorToPackageId loc
  let pkg = T.pack $ Pretty.prettyShow $ PackageIdentifier pname pid
  let actualPath = T.intercalate "/" $ pkg : ps

  -- Now get offsets for everything in the tarball that is under the requested
  -- path.
  mstuff <- liftDB $ doSelect $ do
    tar <- getLatestTarball loc
    off <- each tarIndexSchema
    where_ $ tarIndexBlob off ==. tarballBlobNoGz tar
    -- Look only for files whose path starts with @actualPath@. In principle
    -- this could incorrectly interpret the final path segment as a prefix
    -- glob, but that doesn't actuall occur due to the 303 redirect discussed
    -- below.
    where_ $ startsWith (tarIndexPath off) $ lit actualPath
    pure ((tarballBlobNoGz tar, tarIndexOffset off), tarIndexPath off)

  -- Branch on what's going on:
  case mstuff of
    -- We didn't find anything under the given path, so return 404.
    [] -> throwError err404

    -- We found a single file under the given path. Since we've only done
    -- a prefix check, now determine whether the file is exactly the requested
    -- path. If so, we can serve the file. If not, it's a false positive and we
    -- should still return 404.
    [((blob, off), path)]
      | path == actualPath -> do
          -- Lookup the file in the tarball...
          store <- asks serverBlobStore
          liftIO (loadTarEntry_ (Blob.filepath store blob) off) >>= \case
            Right (_, e) ->
              -- ...and serve it as plaintext.
              pure $ HHere Proxy $ toStrict $ decodeUtf8 e
            Left _ -> throwError err500
      | otherwise -> throwError err404

    -- Otherwise we have many matches and should serve a directory listing.
    _ ->
      -- Check if there is an empty segment in the request path. This is
      -- desirable for two reasons:
      --
      -- 1. If the current URL ends in a slash (eg "blah/"), browsers resolve
      --    bare URIs (eg "ex") as child resources (eg "blah/ex").
      -- 2. By inserting an empty final segment, our 'startsWith' sql query
      --    ends in a @/@, and therefore doesn't perform accidental prefix
      --    matches on directory names.
      --
      -- If there isn't an empty final path segment, we want to 303 redirect to
      -- it.
      case List.isSuffixOf [""] ps of
        False -> throwError err303
          { errHeaders = pure
              ( hLocation
              , mappend "/" $ toHeader $ fieldLink pkgdb_api_tarballContent loc $ ps <> [""]
              )
          }
        True -> do
          -- Finally, if we've made it here, we have a real set of files
          -- underneath the requested path. We can serve this as an HTML
          -- directory listing.
          pure $ HThere $ HHere Proxy $ DirectoryListing $ mconcat $ do
            (_, pathp) <- mstuff
            -- The paths we found have the entire request path as a prefix.
            -- Since we only want relative paths at this point, we must strip
            -- off that prefix.
            Just path <- pure $ T.stripPrefix actualPath pathp
            -- And then eliminate any directories from the file listing, since
            -- these automatically get generated by the trie.
            guard $ not $ T.isSuffixOf "/" path
            guard $ path /= mempty
            pure $ pathToTrie $ T.split (== '/') path


loadTarEntry_
  :: FilePath
  -- ^ Tarball
  -> Int64
  -> IO (Either String (Tar.FileSize, BSL.ByteString))
loadTarEntry_ tarfile off = do
  htar <- openFile tarfile ReadMode
  hSeek htar AbsoluteSeek $ fromIntegral $ off * 512
  header <- BSL.hGet htar 512
  case Tar.read header of
    (Tar.Next Tar.Entry{Tar.entryContent = Tar.NormalFile _ size} _) -> do
         body <- BSL.hGet htar (fromIntegral size)
         pure $ Right (size, body)
    z -> pure $ Left $ fail $  "failed to read entry from tar file: " <> show (tarfile, off, show z)

