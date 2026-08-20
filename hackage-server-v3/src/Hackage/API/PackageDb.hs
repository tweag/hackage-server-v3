{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans   #-}

module Hackage.API.PackageDb
  ( packageDbServer
  ) where

import Control.Applicative (empty, optional)
import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Control.Monad (unless, guard)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO, asks)
import Crypto.Hash qualified as Crypto
import Data.Aeson hiding (Result(..))
import Data.BlobStorage qualified as Blob
import Data.Bool (bool)
import Data.ByteString (StrictByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Functor ((<&>))
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import Data.List qualified as List
import Data.Map qualified as M
import Data.Map.Monoidal qualified as MM
import Data.Maybe (listToMaybe)
import Data.Proxy (Proxy(..))
import Data.Set qualified as S
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Arbitrary ()
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Time (UTCTime)
import Data.Trie (pathToTrie, flattenTrie)
import Distribution.License (licenseToSPDX)
import Distribution.PackageDescription.Parsec qualified as PkgDescr
import Distribution.Pretty qualified as Pretty
import Distribution.Types.Dependency as Cabal
import Distribution.Types.GenericPackageDescription qualified as PkgDescr
import Distribution.Types.PackageDescription qualified as PkgDescr
import Distribution.Types.VersionRange (anyVersion)
import Distribution.Utils.ShortText (fromShortText)
import Hackage.API.Query
import Hackage.API.Type
import Hackage.Objects
import Hackage.Schemas.Packages
import Hackage.Schemas.Users
import Hackage.ServerM
import Hackage.TarIndex
import Hackage.Types
import Hackage.Utils
import Network.HTTP.Types.Header (hLocation)
import Rel8 hiding (Lift, bool, filter, optional)
import Rel8 qualified
import Servant.API
import Servant.EDE (HTML, HasTemplate(..), ToObject(..))
import Servant.HackageCombinators.DynamicGet (OneOf(..))
import Servant.HackageCombinators.NegotiableContent (NegotiatedContent)
import Servant.Links (fieldLink)
import Servant.Server (err303, err404, err500, ServerError(..))
import Servant.Server.Generic (AsServerT)
import Servant.Tarball (Tarball)
import System.IO (SeekMode(..), hSeek, IOMode(..), withBinaryFile)


packageDbServer :: PackageDbApi (AsServerT ServerM)
packageDbServer = PackageDbApi
  { pkgdb_api_revisions_redirect = \cType ->
      -- Redirect this route back to 'pkgdb_api_revisions', but having maybe
      -- parsed off a negotiated content type.
      fieldLink pkgdb_api_revisions cType
  , pkgdb_api_revisions = packageRevisions
  , pkgdb_api_revisionMetadata = packageRevisionMetadata
  , pkgdb_api_revisionCabal = packageRevisionCabal
  , pkgdb_api_versions = packageVersions
  , pkgdb_api_cabalFile = packageCabalFile
  , pkgdb_api_metadata = packageMetadata
  , pkgdb_api_preferredVersions = packagePreferredVersions
  , pkgdb_api_uploader = packageUploader
  , pkgdb_api_uploadTime = packageUploadTime
  , pkgdb_api_tarball = packageTarball
  , pkgdb_api_distroMonitor = packageDistroMonitor
  , pkgdb_api_dependencies = packageDependencies
  , pkgdb_api_deprecated = packageDeprecation
  , pkgdb_api_allDeprecated = packageAllDeprecations
  , pkgdb_api_tarballContent = packageTarballContent
  , pkgdb_api_docs = packageDocsContent
  , pkgdb_api_docsTarball = packageDocsTarball
  , pkgdb_api_changelog = packageChangelog
  }


--------------------------------------------------------------------------------
-- /package/:packagename.json

packageVersions :: PackageName -> ServerM PackageVersions
packageVersions pname = do
  versions <- runDB $ doSelect $ do
    pkgv <- getAllVersions $ lit pname
    pure (packageVersion pkgv, pkgInfoDeprecated pkgv)
  pure $ PackageVersions $ M.fromList $ fmap (fmap $ bool Normal Deprecated) versions


--------------------------------------------------------------------------------
-- /package/:packageid.json

cabalToDTO :: MetadataRevisionRow Result -> UserName -> ServerM PackageBasicDescriptionDTO
cabalToDTO rev user = do
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


packageMetadata :: PackageId -> ServerM PackageBasicDescriptionDTO
packageMetadata pid = do
  (rev, user) <- runDB $ doSelect1 $ do
    rev <- getLatestRev $ Specific pid
    user <- each usersSchema
    where_ $ userId user ==. metadataUploader rev
    pure (rev, userName user)
  cabalToDTO rev user


--------------------------------------------------------------------------------
-- /package/:packageid/revision/:rev[.json]

packageRevisionMetadata
    :: Maybe NegotiatedContent
    -> PackageId
    -> MetadataRevIx
    -> ServerM PackageBasicDescriptionDTO
packageRevisionMetadata _ pid revix = do
  (rev, user) <- runDB $ doSelect1 $ do
    rev <- getAllRevs $ Specific pid
    where_ $ metadataRevId rev ==. lit revix
    user <- each usersSchema
    where_ $ userId user ==. metadataUploader rev
    pure (rev, userName user)
  cabalToDTO rev user


--------------------------------------------------------------------------------
-- /package/:packageid/revision/:rev.cabal

packageRevisionCabal
    :: PackageId
    -> MetadataRevIx
    -> ServerM StrictByteString
packageRevisionCabal pid revix = do
  (rev) <- runDB $ doSelect1 $ do
    rev <- getAllRevs $ Specific pid
    where_ $ metadataRevId rev ==. lit revix
    pure rev
  pure $ metadataCabalFile rev


--------------------------------------------------------------------------------
-- /package/:package/:package.cabal


packageCabalFile :: PackageLocator -> PackageName -> ServerM StrictByteString
packageCabalFile loc pname2 = do
  unless (packageLocName loc == pname2) $ throwError err404
  runDB $ doSelect1 $ do
    rev <- getLatestRev loc
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
  versions <- runDB $ doSelect $ do
    pkgv <- getAllVersions $ lit pname
    pure (packageVersion pkgv, pkgInfoDeprecated pkgv)
  pure
    $ WithPackageName pname
    $ PreferredVersions
    $ M.fromList
    $ fmap (fmap $ bool Normal Deprecated) versions


--------------------------------------------------------------------------------
-- /package/:package/deprecated

instance ToObject Deprecation where
  toObject (Deprecation Nothing) =
    [ "deprecated" .= False
    ]
  toObject (Deprecation (Just deprs)) =
    [ "deprecatedFor" .= S.toList deprs
    , "deprecated" .= True
    ]

instance HasTemplate HTML Deprecation where
  templateFor _ _ = "packages/deprecated.html"


packageDeprecation
    :: Maybe NegotiatedContent
    -> PackageName
    -> ServerM (WithPackageName Deprecation)
packageDeprecation _ pname = do
  -- TODO(sandy): share the db connection
  isDepr <- runDB $ doSelect1 $ do
    pkg <- each packageNameSchema
    where_ $ packageName pkg ==. lit pname
    pure $ packageDeprecated pkg
  case isDepr of
    False ->
      pure $ WithPackageName pname $ Deprecation Nothing
    True -> do
      deprs <- runDB $ doSelect $ do
        pkg <- each packageNameSchema
        where_ $ packageName pkg ==. lit pname
        depr <- each pkgDeprecationSchema
        where_ $ packageNameId pkg ==. pkgDeprecatedPkg depr
        deprFor <- each packageNameSchema
        where_ $ packageNameId deprFor ==. pkgDeprecatedInFavorOf depr
        pure $ packageName deprFor
      pure $ WithPackageName pname $ Deprecation $ Just $ S.fromList deprs


--------------------------------------------------------------------------------
-- /packages/deprecated

instance ToObject AllDeprecations where
  toObject (AllDeprecations deps) =
    [ "deprecations" .= do
        (pkg, replacements) <- M.toList deps
        pure $ object
          [ "pkg" .= pkg
          , "replacements" .= S.toList replacements
          ]
    ]

instance HasTemplate HTML AllDeprecations where
  templateFor _ _ = "packages/allDeprecated.html"


packageAllDeprecations
    :: Maybe NegotiatedContent
    -> ServerM AllDeprecations
packageAllDeprecations _ = do
  deprs <- runDB $ doSelect $ do
    pkg <- each packageNameSchema
    where_ $ packageDeprecated pkg
    depr <- each pkgDeprecationSchema
    where_ $ packageNameId pkg ==. pkgDeprecatedPkg depr
    deprFor <- each packageNameSchema
    where_ $ packageNameId deprFor ==. pkgDeprecatedInFavorOf depr
    pure $ (packageName pkg, packageName deprFor)
  pure $ AllDeprecations $ MM.getMonoidalMap $ mconcat $ do
    (pkg, depr) <- deprs
    pure $ MM.singleton pkg $ S.singleton depr


--------------------------------------------------------------------------------
-- /package/:package/uploader

packageUploader :: PackageLocator -> ServerM UserName
packageUploader pname =
  runDB $ doSelect1 $ do
    pkgv <- onlyLatestRev $ getAllRevs pname
    u <- each usersSchema
    where_ $ metadataUploader pkgv ==. userId u
    pure $ userName u


--------------------------------------------------------------------------------
-- /package/:package/upload-time

packageUploadTime :: PackageLocator -> ServerM UTCTime
packageUploadTime pname =
  runDB $ doSelect1 $ do
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

  mblob
    <- runDB
     $ doSelect1
     $ Rel8.optional
     $ fmap tarballBlobGz
     $ getLatestTarball
     $ Specific tarball
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
packageDistroMonitor _ pname
  = fmap (WithPackageName pname . AllTarballs)
  $ fmap (fmap (uncurry PackageIdentifier))
  $ runDB
  $ doSelect
  $ orderBy (snd >$< asc) $ do
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
  rev <- runDB $ doSelect1 $ do
    pkgv <- onlyLatestRev $ getAllRevs pname
    pure pkgv

  let parseResult = PkgDescr.parseGenericPackageDescription $ metadataCabalFile rev
  case PkgDescr.runParseResult parseResult of
    (_, Right pkg) -> do
      let pkgd = PkgDescr.packageDescription pkg
      pure
        $ WithPackage (PkgDescr.package pkgd)
        $ Dependencies False
        $ PkgDescr.allBuildDepends pkgd
    _ -> throwError err500


--------------------------------------------------------------------------------
-- /package/:package/revisions

instance HasTemplate HTML Revisions where
  templateFor _ _ = "packages/revisions.html"

instance ToObject Revisions where
  toObject (Revisions revs) =
    [ "revisions" .= revs
    ]


packageRevisions
    :: Maybe NegotiatedContent
    -> PackageLocator
    -> ServerM (WithPackage Revisions)
packageRevisions _ loc = do
  (name, version) <- runDB $ doSelect1 $ locatorToPackageId loc
  revs <- runDB $ doSelect $ do
    rev <- getAllRevs loc
    u <- each usersSchema
    where_ $ userId u ==. metadataUploader rev
    pure (rev, userName u)
  pure
    $ WithPackage (PackageIdentifier name version)
    $ Revisions
    $ revs <&> \(rev, user) ->
        Revision
          { number = metadataRevId rev
          , sha256
              = T.pack
              $ show
              $ Crypto.hashWith Crypto.SHA256
              $ metadataCabalFile rev
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

serveTarballContent
  :: ([Text] -> Link)
  -> Text
  -- ^ Path prefix
  -> PackageIdentifier
  -> BlobId Tarball
  -> [Text]
  -> ServerM (OneOf '[ '(PlainText, Text)
                     , '(HTML, WithPackage DirectoryListing)
                     ])
serveTarballContent mklink prefix pkg blob ps = do
  -- Since all the paths in the package tarballs are prefixed by their pretty
  -- packageid, we must first resolve the locator.
  let actualPath = T.intercalate "/" $ prefix : ps
  store <- asks serverBlobStore

  -- Now get offsets for everything in the tarball that is under the requested
  -- path.
  mstuff <- runDB $
    indexingTarIndices
      store
      (pure $ lit blob) $ \_ off -> do
        -- Look only for files whose path starts with @actualPath@. In principle
        -- this could incorrectly interpret the final path segment as a prefix
        -- glob, but that doesn't actuall occur due to the 303 redirect discussed
        -- below.
        where_ $ startsWith (tarIndexPath off) $ lit actualPath
        pure (tarIndexOffset off, tarIndexPath off)

  let actuallyFound = listToMaybe $ filter ((== actualPath) . snd) mstuff
      isDir = T.isSuffixOf "/" actualPath || all (T.isPrefixOf (actualPath <> "/") . snd) mstuff

  -- Branch on what's going on:
  case mstuff of
    -- We didn't find anything at all under the given path, so return 404.
    [] -> throwError err404

    -- We found a file at exactly the requested path, and it is not a directory.
    _ | Just (off, _) <- actuallyFound
      , not isDir -> do
      -- Lookup the file in the tarball...
      liftIO (loadTarEntry_ (Blob.filepath store blob) off) >>= \case
        Right (_, e) ->
          -- ...and serve it as plaintext.
          pure $ HHere Proxy $ toStrict $ decodeUtf8 e
        Left _ -> throwError err500

    -- If we're not looking at a directory, there is no such file and
    -- we can return a 404.
    _ | not isDir -> throwError err404

    -- Otherwise we have many matches and none of them is the one we're looking
    -- for. Thus we should serve a directory listing.
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
              , mappend "/" $ toHeader $ mklink $ ps <> [""]
              )
          }
        True -> do
          -- Finally, if we've made it here, we have a real set of files
          -- underneath the requested path. We can serve this as an HTML
          -- directory listing.
          pure $ HThere $ HHere Proxy $ WithPackage pkg $ DirectoryListing $ mconcat $ do
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


--------------------------------------------------------------------------------
-- /package/:package/src/...

packageTarballContent
    :: PackageLocator
    -> [Text]
    -> ServerM (OneOf '[ '(PlainText, Text)
                       , '(HTML, WithPackage DirectoryListing)
                       ])
packageTarballContent loc ps = do
  (pname, pid) <- runDB $ doSelect1 $ locatorToPackageId loc

  blob <- runDB $ doSelect1 $ do
    tar <- getLatestTarball loc
    pure $ tarballBlobNoGz tar

  serveTarballContent
    (fieldLink pkgdb_api_tarballContent loc)
    (T.pack $ Pretty.prettyShow $ PackageIdentifier pname pid)
    (PackageIdentifier pname pid)
    blob
    ps


getLatestDocs
  :: PackageLocator
  -> Query (Expr Version, Expr (BlobId Tarball))
getLatestDocs loc = limit 1 $
  case loc of
    Specific _ -> do
      pkginfo <- getLocator loc
      doc <- each pkgDocsSchema
      where_ $ pkgDocsPkg doc ==. pkgInfoId pkginfo
      pure (packageVersion pkginfo, pkgDocsTarball doc)
    Latest name -> latestBy fst $ do
      -- When no specific version is given, find the latest version that has
      -- any documentation at all.
      pkg <- each packageNameSchema
      where_ $ packageName pkg ==. lit name
      pkginfo <- each pkgInfoSchema
      where_ $ pkgId pkginfo ==. packageNameId pkg
      doc <- each pkgDocsSchema
      where_ $ pkgDocsPkg doc ==. pkgInfoId pkginfo
      pure (packageVersion pkginfo, pkgDocsTarball doc)

--------------------------------------------------------------------------------
-- /package/:package/docs.tar

packageDocsTarball
    :: PackageLocator
    -> ServerM BSL.LazyByteString
packageDocsTarball loc = do
  (_, blob) <- runDB $ doSelect1 $ getLatestDocs loc
  store <- asks serverBlobStore
  liftIO $ Blob.get store blob


--------------------------------------------------------------------------------
-- /package/:package/docs/...

packageDocsContent
    :: PackageLocator
    -> [Text]
    -> ServerM (OneOf '[ '(PlainText, Text)
                       , '(HTML, WithPackage DirectoryListing)
                       ])
packageDocsContent loc ps = do
  (version, blob) <- runDB $ doSelect1 $ getLatestDocs loc
  serveTarballContent
    (fieldLink pkgdb_api_tarballContent loc)
    (mconcat
      [ T.pack
          $ Pretty.prettyShow
          $ PackageIdentifier (packageLocName loc) version
      , "-docs"
      ])
    (PackageIdentifier (packageLocName loc) version)
    blob
    ps


loadTarEntry_
  :: FilePath
  -- ^ Tarball
  -> Int64
  -> IO (Either String (Tar.FileSize, BSL.ByteString))
loadTarEntry_ tarfile off = withBinaryFile tarfile ReadMode $ \htar -> do
  hSeek htar AbsoluteSeek $ fromIntegral $ off * 512
  header <- BS.hGet htar 512
  case Tar.read $ BSL.fromStrict header of
    (Tar.Next Tar.Entry{Tar.entryContent = Tar.NormalFile _ size} _) -> do
         body <- BS.hGet htar (fromIntegral size)
         pure $ Right (size, BSL.fromStrict body)
    z -> pure
       $ Left
       $ fail
       $ "failed to read entry from tar file: " <> show (tarfile, off, show z)


--------------------------------------------------------------------------------
-- /package/:package/changelog

packageChangelog
  :: Maybe NegotiatedContent
  -> PackageLocator
  -> ServerM (WithPackage Changelog)
packageChangelog _ loc = do
  store <- asks serverBlobStore
  mres <- runDB $ optional $ do
    (pname, version) <- doSelect1 $ locatorToPackageId loc
    let pkg = PackageIdentifier pname version
    mres <-
      indexingTarIndices store
        ( do
            tar <- getLatestTarball loc
            pure $ tarballBlobNoGz tar
        )
        ( \blob idx -> do
            let prefix = mconcat
                  [ lit $ T.pack $ Pretty.prettyShow pkg
                  , "/"
                  ]
            where_ $ in_ (tarIndexPath idx) $ id @[_] $ do
              base <- [ "news", "changelog", "change_log", "changes"
                      , "NEWS", "CHANGELOG", "CHANGE_LOG", "CHANGES"
                      , "News", "Changelog", "Change_log", "Changes"
                              , "ChangeLog", "Change_Log"
                      ]
              ext <- [ "", ".txt", ".md", ".markdown"
                    ,     ".TXT", ".MD", ".MARKDOWN"
                    ]
              pure $ prefix <> base <> ext
            pure (blob, tarIndexOffset idx)
        )
    case listToMaybe mres of
      Just (blob, off) -> pure (pkg, blob, off)
      Nothing -> empty
  case mres of
    Just (pkg, blob, off) -> do
      liftIO (loadTarEntry_ (Blob.filepath store blob) off) >>= \case
        Right (_, e) ->
          pure
            $ WithPackage pkg
            $ Changelog
            $ toStrict
            $ decodeUtf8 e
        Left _ -> throwError err500
    Nothing -> do
      -- NOTE: v2 returns a 200 with the text "Changelog not found" here.
      throwError err404

instance ToObject Changelog where
  toObject (Changelog c) =
    [ "changelog" .= c ]

instance HasTemplate HTML Changelog where
  templateFor _ _ = "packages/changelog.html"

