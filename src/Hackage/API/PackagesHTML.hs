{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Hackage.API.PackagesHTML
  ( PackagesHtmlAPI (..)
  , packagesHtmlServer
  ) where


import Data.ByteString (StrictByteString)
import Distribution.Utils.MD5 (md5, showMD5)
import Distribution.Types.VersionRange (anyVersion)
import Distribution.Types.Dependency as Cabal
import Control.Monad.Reader
import Hackage.ServerM
import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Data.Aeson hiding (Result(..))
import Data.BlobStorage qualified as Blob
import Data.Bool
import Data.ByteString.Lazy qualified as BL
import Data.Coerce
import Data.Functor
import Data.Functor.Contravariant
import Data.Hashable
import Data.Kind (Type)
import Data.List (partition)
import Data.Map (Map)
import Data.Map qualified as M
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Arbitrary ()
import Data.Time (UTCTime)
import Distribution.License (licenseToSPDX)
import Distribution.PackageDescription.Parsec qualified as PkgDescr
import Distribution.Pretty qualified as Pretty
import Distribution.SPDX.License (License)
import Distribution.Types.GenericPackageDescription qualified as PkgDescr
import Distribution.Types.PackageDescription qualified as PkgDescr
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Utils.ShortText (fromShortText)
import GHC.Generics (Generic)
import GHC.TypeLits
import Hackage.Schemas.Packages
import Hackage.Schemas.Users
import Hackage.Types
import Hackage.Utils
import Rel8 hiding (Lift, bool)
import Servant.API
import Servant.EDE
import Servant.HackageCombinators.CaptureExt
import Servant.HackageCombinators.NegotiableContent
import Servant.Server (err404, err500)
import Servant.Server.Generic (AsServerT)
import Servant.Tarball
import Test.QuickCheck
import Hackage.Objects

data PackagesHtmlAPI mode = PackagesHtmlAPI
  { htmlPackagesNames :: mode
      :- "packages"
      :> "names"
      :> Get '[HTML] PackageNames
  , htmlPackagesTrustees :: mode
      :- "packages"
      :> "trustees"
      :> Get '[HTML] TrusteesObject
  , htmlPackagesHelp :: mode
      :- "upload" :> Get '[HTML] UploadHelp
  , htmlPackagesUploadForm :: mode
      :- "packages"
      :> "upload"
      :> Get '[HTML] PackageUpload
  , htmlPackageRevisions :: mode
      :- NegotiableContent
      :> "package"
      :> Capture "package" PackageLocator
      :> "revisions"
      :> Get '[HTML, JSON] (WithPackage Revisions)
  , htmlTarball :: mode
      :- "packages"
      :> Capture "package" PackageLocator
      :> CaptureExt "tarball" PackageIdentifier "tar.gz"
      :> Get '[Tarball] BL.ByteString
  , htmlTarballs :: mode
      :- NegotiableContent
      :> "package"
      :> Capture "package" PackageName
      :> "distro-monitor"
      :> Get '[HTML] (WithPackageName AllTarballs)
  , htmlMirrorUploader :: mode
      :- "package"
      :> Capture "package" PackageLocator
      :> "uploader"
      :> Get '[PlainText] UserName
  , htmlMirrorUploadTime :: mode
      :- "package"
      :> Capture "package" PackageLocator
      :> "upload-time"
      :> Get '[PlainText] UTCTime
  , htmlPackageDeps :: mode
      :- "package"
      :> Capture "package" PackageLocator
      :> "dependencies"
      :> Get '[HTML] (WithPackage Dependencies)
  , htmlPackageVersions :: mode
      :- "package"
      :> CaptureExt "package" PackageName "json"
      :> Get '[JSON] PackageVersions
  , htmlPackageMetadata :: mode
      :- "package"
      :> CaptureExt "package" PackageIdentifier "json"
      :> Get '[JSON] PackageBasicDescriptionDTO
  , htmlPackageCabalFile :: mode
      :- "package"
      :> Capture "package" PackageName
      :> CaptureExt "package" PackageName "cabal"
      :> Get '[PlainText] StrictByteString
  , htmlPackagePreferredVersions :: mode
      :- NegotiableContent
      :> "package"
      :> Capture "package" PackageName
      :> "preferred"
      :> Get '[HTML, JSON] (WithPackageName PreferredVersions)
  }
  deriving stock (Generic)



packagesHtmlServer :: PackagesHtmlAPI (AsServerT ServerM)
packagesHtmlServer = PackagesHtmlAPI
  { htmlPackagesNames = namesStub
  , htmlPackagesTrustees = trusteesEndpoint
  , htmlPackagesHelp = staticHTML
  , htmlPackageVersions = packageVersionsEndpoint
  , htmlPackagesUploadForm = staticHTML
  , htmlPackageCabalFile = packageCabalFileEndpoint
  , htmlPackageMetadata = packageMetadataEndpoint
  , htmlPackagePreferredVersions = packagePreferredVersionsEndpoint
  , htmlMirrorUploader = packageMirrorUploader
  , htmlMirrorUploadTime = packageMirrorUploadTime
  , htmlTarball = packageTarball
  , htmlTarballs = packageTarballs
  , htmlPackageDeps = packageDependencies
  , htmlPackageRevisions = packageRevisions
  }


--------------------------------------------------------------------------------
-- /packages/names

instance HasTemplate HTML PackageNames where
  templateFor _ _ = "packages/names.html"

data PackageNames = PackageNames
  { packages :: Map Text PackageNameData
  }
  deriving stock (Show, Generic)
  deriving anyclass ToObject

instance Arbitrary PackageNames where
  arbitrary = fmap PackageNames arbitrary


data PackageNameData = PackageNameData
  { pkgDesc :: Text
  , pkgTags :: [Tag]
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

instance Arbitrary PackageNameData where
  arbitrary = PackageNameData <$> arbitrary <*> arbitrary


namesStub :: ServerM PackageNames
namesStub = pure $ PackageNames $ M.fromList
  [ ("hello", PackageNameData "from space" ["a", "b", "c"])
  , ("goodbye", PackageNameData "my dude" ["a", "b"])
  ]

--------------------------------------------------------------------------------
-- /packages/trustees
data TrusteesObject = TrusteesObject (Map UserId UserName)
  deriving stock (Eq, Show, Generic)
  deriving anyclass Hashable

instance Arbitrary TrusteesObject where
  arbitrary = fmap TrusteesObject arbitrary

instance ToJSON TrusteesObject where
  toJSON ts = Object $ toObject ts <>
    [ "title" .= id @String "Package trustees"
    , "description" .= id @String "The role of trustees is to help to curate the whole package collection. Trustees have a limited ability to edit package information, for the entire package database (as opposed to package maintainers who have full control over individual packages). Trustees can edit .cabal files, edit other package metadata and upload documentation but they cannot upload new package versions."
    ]

instance ToObject TrusteesObject where
  toObject (TrusteesObject ts) =
    [ "members" .= (M.toList ts <&> \(uid, name) ->
        object
          [ "userid" .= uid
          , "username" .= name
          ]
      )
    ]

instance HasTemplate HTML TrusteesObject where
  templateFor _ _ = "upload/trustees.html"


trusteesEndpoint :: ServerM TrusteesObject
trusteesEndpoint = do
  ts <- liftDB $ doSelect $ do
    r <- each userRolesSchema
    where_ $ userRoleRole r ==. lit Trustee
    u <- activeUsers
    where_ $ userId u ==. userRoleUserId r
    pure (userRoleUserId r, userName u)

  pure $ TrusteesObject $ M.fromList ts


--------------------------------------------------------------------------------
-- | A @'StaticHTML' template@ uses the statically known type-level symbol
-- @template@ for its template. As suggested by its name, it can be used to
-- serve static HTML templates. In order to use this type, you should @newtype@
-- wrap it, and @newtype@ derive 'Eq', 'Show', 'Arbitrary', 'ToObject', and
-- @'HasTemplate' 'HTML'@. You can get a free handler for it via 'staticHTML'
--
-- The advantage of newtype-wrapping this type is that doing so prevents the
-- template names from leaking into the API contract. Furthermore, it provides
-- a forward-compatable means of making the endpoint /less/ static in the
-- future :)
type StaticHTML :: Symbol -> Type
data StaticHTML template = StaticHTML
  deriving stock (Eq, Show, Generic)
  deriving anyclass Hashable

instance Arbitrary (StaticHTML a) where
  arbitrary = pure StaticHTML

instance ToObject (StaticHTML a) where
  toObject _ = mempty

instance KnownSymbol a => HasTemplate HTML (StaticHTML a) where
  templateFor _ _ = symbolVal @a undefined

-- | Get a handler for a newtype-wrapped 'StaticHTML' value.
staticHTML :: Coercible (StaticHTML a) b => ServerM b
staticHTML = pure $ coerce StaticHTML


--------------------------------------------------------------------------------
-- /upload
newtype UploadHelp = UploadHelp (StaticHTML "upload/help.html")
  deriving newtype (Eq, Show, Hashable, Arbitrary, ToObject, HasTemplate HTML)

--------------------------------------------------------------------------------
-- /package/upload

newtype PackageUpload = PackageUpload (StaticHTML "upload/form.html")
  deriving newtype (Eq, Show, Hashable, Arbitrary, ToObject, HasTemplate HTML)

--------------------------------------------------------------------------------
-- /package/:packagename.json

data VersionStatus = Normal | Deprecated
  deriving stock (Eq, Ord, Show, Generic)

instance ToJSON VersionStatus where
  toJSON Normal = "normal"
  toJSON Deprecated = "deprecated"

instance Arbitrary VersionStatus where
  arbitrary = elements [Normal, Deprecated]

data PackageVersions = PackageVersions
  { getPackageVersions :: Map Version VersionStatus
  }
  deriving stock (Eq, Ord, Show, Generic)

instance Arbitrary PackageVersions where
  arbitrary = fmap PackageVersions arbitrary

instance ToJSON PackageVersions where
  toJSON
    = object
    . fmap (\(v, s) -> fromString (Pretty.prettyShow v) .= s)
    . M.toList
    . getPackageVersions


packageVersionsEndpoint :: PackageName -> ServerM PackageVersions
packageVersionsEndpoint pname = do
  versions <- liftDB $ doSelect $ do
    pkg <- each packageNameSchema
    where_ $ packageName pkg ==. lit pname
    pkgv <- each pkgInfoSchema
    where_ $ pkgId pkgv ==. packageNameId pkg
    pure (packageVersion pkgv, pkgInfoDeprecated pkgv)
  pure $ PackageVersions $ M.fromList $ fmap (fmap $ bool Normal Deprecated) versions


--------------------------------------------------------------------------------
-- /package/:packageid.json


getAllRevs :: PackageIdentifier -> Query (MetadataRevisionRow Expr)
getAllRevs pid = do
  pkg <- each packageNameSchema
  where_ $ packageName pkg ==. lit (pkgName pid)
  pkgv <- each pkgInfoSchema
  where_ $ pkgId pkgv ==. packageNameId pkg
  where_ $ packageVersion pkgv ==. lit (pkgVersion pid)
  rev <- each metadataRevisionsSchema
  where_ $ metadataPkgId rev ==. pkgInfoId pkgv
  pure rev

getLatestRev :: PackageId -> Query (MetadataRevisionRow Expr)
getLatestRev = onlyLatestRev . getAllRevs


onlyLatestRev :: Query (MetadataRevisionRow Expr) -> Query (MetadataRevisionRow Expr)
onlyLatestRev = limit 1 . orderBy (metadataTime >$< desc)


data PackageBasicDescriptionDTO = PackageBasicDescriptionDTO
  { license           :: !License
  , copyright         :: !Text
  , synopsis          :: !Text
  , description       :: !Text
  , author            :: !Text
  , homepage          :: !Text
  , metadata_revision :: !MetadataRevIx
  , uploaded_at       :: !UTCTime
  , uploader          :: !UserName
  } deriving stock (Eq, Show, Generic)


instance ToJSON PackageBasicDescriptionDTO where
  toJSON dto =
    object
      [ "license"           .= Pretty.prettyShow (license dto)
      , "copyright"         .= copyright dto
      , "synopsis"          .= synopsis dto
      , "description"       .= description dto
      , "author"            .= author dto
      , "homepage"          .= homepage dto
      , "metadata_revision" .= metadata_revision dto
      , "uploaded_at"       .= uploaded_at dto
      , "uploader"          .= uploader dto
      ]

packageMetadataEndpoint :: PackageId -> ServerM PackageBasicDescriptionDTO
packageMetadataEndpoint pid = do
  (rev, user) <- liftDB $ doSelect1 $ do
    rev <- getLatestRev pid
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

getLatestVersionAndRev :: Expr PackageName -> Query (MetadataRevisionRow Expr)
getLatestVersionAndRev = onlyLatestRev . getLatestVersionRevs

getLatestVersion :: Expr PackageName -> Query (PkgInfoRow Expr)
getLatestVersion pname =
  limit 1 $ orderBy (packageVersion >$< desc) $ do
    pkg <- each packageNameSchema
    where_ $ packageName pkg ==. pname
    pkgv <- each pkgInfoSchema
    where_ $ pkgId pkgv ==. packageNameId pkg
    pure pkgv

getLatestVersionRevs :: Expr PackageName -> Query (MetadataRevisionRow Expr)
getLatestVersionRevs pname = do
  version <- getLatestVersion pname
  rev <- each metadataRevisionsSchema
  where_ $ metadataPkgId rev ==. pkgInfoId version
  pure rev



packageCabalFileEndpoint :: PackageName -> PackageName -> ServerM StrictByteString
packageCabalFileEndpoint pname1 pname2 = do
  -- For legacy reasons, this path requires both package names to be the same
  unless (pname1 == pname2) $ throwError err404
  liftDB $ doSelect1 $ do
    rev <- getLatestVersionAndRev $ lit pname1
    pure $ metadataCabalFile rev


--------------------------------------------------------------------------------
-- /package/:package/preferred

newtype PreferredVersions = PreferredVersions
  { getPreferredVersions :: Map Version VersionStatus
  }
  deriving stock (Eq, Ord, Show, Generic)

instance Arbitrary PreferredVersions where
  arbitrary = PreferredVersions <$> arbitrary

instance ToJSON PreferredVersions where
  toJSON (PreferredVersions vs) = do
    let (normal, deprecated) = partition ((== Normal) . snd) $ M.toList vs
    object
      [ "normal-version" .= fmap (Pretty.prettyShow . fst) normal
      , "deprecated-version" .= fmap (Pretty.prettyShow . fst) deprecated
      ]

instance ToObject PreferredVersions where
  toObject (PreferredVersions vs) =
      [ "versions" .= M.mapKeys Pretty.prettyShow vs
      ]

instance HasTemplate HTML PreferredVersions where
  templateFor _ _ = "packages/preferred.html"

packagePreferredVersionsEndpoint :: PackageName -> ServerM (WithPackageName PreferredVersions)
packagePreferredVersionsEndpoint pname = do
  versions <- liftDB $ doSelect $ do
    pkg <- each packageNameSchema
    where_ $ packageName pkg ==. lit pname
    pkgv <- each pkgInfoSchema
    where_ $ pkgId pkgv ==. packageNameId pkg
    pure (packageVersion pkgv, pkgInfoDeprecated pkgv)
  pure $ WithPackageName pname $ PreferredVersions $ M.fromList $ fmap (fmap $ bool Normal Deprecated) versions


--------------------------------------------------------------------------------
-- /package/:package/uploader

lookupLocatorRevs :: PackageLocator -> Query (MetadataRevisionRow Expr)
lookupLocatorRevs (Latest x) = getLatestVersionRevs $ lit x
lookupLocatorRevs (Specific x) = getAllRevs x

packageMirrorUploader :: PackageLocator -> ServerM UserName
packageMirrorUploader pname =
  liftDB $ doSelect1 $ do
    pkgv <- onlyLatestRev $ lookupLocatorRevs pname
    u <- each usersSchema
    where_ $ metadataUploader pkgv ==. userId u
    pure $ userName u


--------------------------------------------------------------------------------
-- /package/:package/upload-time

packageMirrorUploadTime :: PackageLocator -> ServerM UTCTime
packageMirrorUploadTime pname =
  liftDB $ doSelect1 $ do
    pkgv <- onlyLatestRev $ lookupLocatorRevs pname
    pure $ metadataTime pkgv


--------------------------------------------------------------------------------
-- /package/:package/:tarball.tar.gz

getLatestTarball :: PackageIdentifier -> Query (TarballRevisionRow Expr)
getLatestTarball pid = do
  pkg <- each packageNameSchema
  where_ $ packageName pkg ==. lit (pkgName pid)
  pkgv <- each pkgInfoSchema
  where_ $ pkgId pkgv ==. packageNameId pkg
  where_ $ packageVersion pkgv ==. lit (pkgVersion pid)
  limit 1 $ orderBy (tarballTime >$< desc) $ do
    rev <- each packageTarballRevisionsSchema
    where_ $ tarballPkgId rev ==. pkgInfoId pkgv
    pure rev

packageTarball
    :: PackageLocator
    -> PackageIdentifier
    -> ServerM BL.ByteString
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
    liftDB $ doSelect1 $ optional $ fmap tarballBlobGz $ getLatestTarball tarball
  case mblob of
    Just blob -> do
      store <- asks serverBlobStore
      liftIO $ Blob.get store blob
    Nothing -> throwError err404

--------------------------------------------------------------------------------
-- /package/:package/distro-monitor[.html]

newtype AllTarballs = AllTarballs
  { allTarballs :: [PackageIdentifier]
  }
  deriving stock (Eq, Ord, Show)

instance ToObject AllTarballs where
  toObject (AllTarballs tbs) =
    [ "versions" .= fmap Pretty.prettyShow tbs
    ]

instance Arbitrary AllTarballs where
  arbitrary = AllTarballs <$> arbitrary

instance HasTemplate HTML AllTarballs where
  templateFor _ _ = "packages/distro-monitor.html"

packageTarballs
    :: PackageName
    -> ServerM (WithPackageName AllTarballs)
packageTarballs pname = do
  fmap (WithPackageName pname . AllTarballs . fmap (uncurry PackageIdentifier)) $ liftDB $ doSelect $ orderBy (snd >$< asc) $ do
    pkg <- each packageNameSchema
    where_ $ packageName pkg ==. lit pname
    pkgv <- each pkgInfoSchema
    where_ $ pkgId pkgv ==. packageNameId pkg
    pure (packageName pkg, packageVersion pkgv)


--------------------------------------------------------------------------------
-- /package/:package/dependencies


data Dependencies = Dependencies
  { isCandidate :: Bool
  , dependencies :: [Cabal.Dependency]
  }
  deriving stock (Eq, Ord, Show)

instance Arbitrary Dependencies where
  arbitrary = Dependencies <$> arbitrary <*> arbitrary

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
    pkgv <- onlyLatestRev $ lookupLocatorRevs pname
    pure pkgv

  let parseResult = PkgDescr.parseGenericPackageDescription $ metadataCabalFile rev
  case PkgDescr.runParseResult parseResult of
    (_, Right pkg) -> do
      let pkgd = PkgDescr.packageDescription pkg
      pure $ WithPackage (PkgDescr.package pkgd) $ Dependencies False $ PkgDescr.allBuildDepends pkgd
    _ -> throwError $ err500


--------------------------------------------------------------------------------
-- /package/:package/revisions

newtype Revisions = Revisions
  { _unRevisions :: [Revision]
  }
  deriving newtype (Eq, Ord, Show, Arbitrary)

instance HasTemplate HTML Revisions where
  templateFor _ _ = "packages/revisions.html"


data Revision = Revision
  { number :: MetadataRevIx
  , sha256 :: Text
  , time :: UTCTime
  , user :: UserName
  }
  deriving stock (Eq, Ord, Show)

instance Arbitrary Revision where
  arbitrary = Revision <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance ToObject Revisions where
  toObject (Revisions revs) =
    [ "revisions" .= revs
    ]

instance ToJSON Revisions where
  toJSON (Revisions revs) = toJSON revs

instance ToJSON Revision where
  toJSON rev = object
    [ "number" .= number rev
    , "sha256" .= sha256 rev
    , "time"   .= time rev
    , "user"   .= user rev
    ]

lookupLocatorAllRevs :: PackageLocator -> Query (MetadataRevisionRow Expr)
lookupLocatorAllRevs (Latest x) = getLatestVersionRevs $ lit x
lookupLocatorAllRevs (Specific x) = getAllRevs x


locatorToPackageId :: PackageLocator -> Query (Expr PackageName, Expr Version)
locatorToPackageId (Specific p) = pure $ lit (pkgName p, pkgVersion p)
locatorToPackageId (Latest p) = do
  version <- getLatestVersion $ lit p
  pure (lit p, packageVersion version)


packageRevisions :: PackageLocator -> ServerM (WithPackage Revisions)
packageRevisions loc = do
  (name, version) <- liftDB $ doSelect1 $ locatorToPackageId loc
  revs <- liftDB $ doSelect $ do
    rev <- lookupLocatorRevs loc
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

