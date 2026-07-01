{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Hackage.API.PackagesHTML where

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
import Data.Text.Encoding (encodeUtf8)
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

-- `/packages/.:format`                                   | GET    | html    | html                     |
-- `/packages/.:format`                                   | POST   | html    | html                     |
-- `/packages/browse`                                     | GET    | html    | html                     |
-- `/packages/deprecated.:format`                         | GET    | html    | html                     |
-- `/packages/graph`                                      | GET    | html    | html                     |
-- `/packages/graph.json`                                 | GET    | json    | html                     |
-- `/packages/names`                                      | GET    | html    | html                     |
-- `/packages/preferred.:format`                          | GET    | html    | html                     |
-- `/packages/recent.:format`                             | GET    | html    | html                     |
-- `/packages/recent.:format`                             | GET    | rss     | html                     |
-- `/packages/recent/revisions.:format`                   | GET    | html    | html                     |
-- `/packages/recent/revisions.:format`                   | GET    | rss     | html                     |
-- `/packages/reverse.:format`                            | GET    | html    | html                     |
-- `/packages/search.:format`                             | GET    | html    | html                     |
-- `/packages/tag/:tag.:format`                           | GET    | html    | html                     |
-- `/packages/tag/:tag/alias`                             | PUT    | html    | html                     |
-- `/packages/tag/:tag/alias/edit`                        | GET    | html    | html                     |
-- `/packages/tags/.:format`                              | GET    | html    | html                     |
-- `/packages/top.:format`                                | GET    | html    | html                     |
data PackagesHtmlAPI mode = PackagesHtmlAPI
    -- { htmlPackagesGet :: mode :- "packages" :> Get '[HTML] ()
    -- , htmlPackagesPost :: mode :- "packages" :> Post '[HTML] ()
    -- , htmlPackagesBrowse :: mode :- "packages" :> "browse" :> Get '[HTML] ()
    -- , htmlPackagesDeprecated :: mode :- "packages" :> "deprecated.html" :> Get '[HTML] ()
    -- , htmlPackagesGraph :: mode :- "packages" :> "graph" :> Get '[HTML] ()
    -- , htmlPackagesGraphJson :: mode :- "packages" :> "graph.json" :> Get '[JSON] ()
    { htmlPackagesNames :: mode :- "packages" :> "names" :> Get '[HTML] PackageNames
    , htmlPackagesTrustees :: mode :- "packages" :> "trustees" :> Get '[HTML] TrusteesObject
    , htmlPackagesHelp :: mode :- "upload" :> Get '[HTML] UploadHelp
    , htmlPackagesUploadForm :: mode :- "packages" :> "upload" :> Get '[HTML] PackageUpload
    -- , htmlPackagesPreferred :: mode :- "packages" :> "preferred.html" :> Get '[HTML] ()
    -- , htmlPackagesRecentHtml :: mode :- "packages" :> "recent.html" :> Get '[HTML] ()
    -- , htmlPackagesRecentRss :: mode :- "packages" :> "recent.rss" :> Get '[RSS] ()
    -- , htmlPackagesRecentRevisionsHtml :: mode :- "packages" :> "recent" :> "revisions.html" :> Get '[HTML] ()
    -- , htmlPackagesRecentRevisionsRss :: mode :- "packages" :> "recent" :> "revisions.rss" :> Get '[RSS] ()
    -- , htmlPackagesReverse :: mode :- "packages" :> "reverse.html" :> Get '[HTML] ()
    -- , htmlPackagesSearch :: mode :- "packages" :> "search.html" :> Get '[HTML] ()
    -- , htmlPackagesTagGet :: mode :- "packages" :> "tag" :> CaptureExt "tag" Tag "html" :> Get '[HTML] ()
    -- , htmlPackagesTagAliasPut :: mode :- "packages" :> "tag" :> Capture "tag" Tag :> "alias" :> Put '[HTML] ()
    -- , htmlPackagesTagAliasEdit :: mode :- "packages" :> "tag" :> Capture "tag" Tag :> "alias" :> "edit" :> Get '[HTML] ()
    -- , htmlPackagesTagsGet :: mode :- "packages" :> "tags" :> Get '[HTML] ()
    -- , htmlPackagesTop :: mode :- "packages" :> "top.html" :> Get '[HTML] ()
    , htmlTarball :: mode :-
        "packages" :> Capture "package" (Either PackageName PackageIdentifier)
          :> CaptureExt "tarball" PackageIdentifier "tar.gz" :> Get '[Tarball] BL.ByteString
    , htmlMirrorUploader :: mode :- "packages" :> Capture "package" (Either PackageName PackageIdentifier) :> "uploader" :> Get '[PlainText] UserName
    , htmlMirrorUploadTime :: mode :- "packages" :> Capture "package" (Either PackageName PackageIdentifier) :> "upload-time" :> Get '[PlainText] UTCTime
    , htmlPackageVersions :: mode :- "packages" :> CaptureExt "package" PackageName "json" :> Get '[JSON] PackageVersions
    , htmlPackageMetadata :: mode :- "packages" :> CaptureExt "package" PackageIdentifier "json" :> Get '[JSON] PackageBasicDescriptionDTO
    , htmlPackageCabalFile :: mode :- "packages" :> Capture "package" PackageName :> CaptureExt "package" PackageName "cabal" :> Get '[PlainText] Text
    , htmlPackagePreferredVersions :: mode :-
        NegotiableContent :> "packages" :> Capture "package" PackageName :> "preferred" :> Get '[HTML, JSON] PreferredVersions
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

getLatestRev :: PackageIdentifier -> Query (MetadataRevisionRow Expr)
getLatestRev pid = do
  pkg <- each packageNameSchema
  where_ $ packageName pkg ==. lit (pkgName pid)
  pkgv <- each pkgInfoSchema
  where_ $ pkgId pkgv ==. packageNameId pkg
  where_ $ packageVersion pkgv ==. lit (pkgVersion pid)
  limit 1 $ orderBy (metadataTime >$< desc) $ do
    rev <- each metadataRevisionsSchema
    where_ $ metadataPkgId rev ==. pkgInfoId pkgv
    pure rev

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

  let parseResult = PkgDescr.parseGenericPackageDescription $ encodeUtf8 $ metadataCabalFile rev
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
getLatestVersionAndRev pname = do
  version <- limit 1 $ orderBy (packageVersion >$< desc) $ do
    pkg <- each packageNameSchema
    where_ $ packageName pkg ==. pname
    pkgv <- each pkgInfoSchema
    where_ $ pkgId pkgv ==. packageNameId pkg
    pure pkgv
  limit 1 $ orderBy (metadataTime >$< desc) $ do
    rev <- each metadataRevisionsSchema
    where_ $ metadataPkgId rev ==. pkgInfoId version
    pure rev



packageCabalFileEndpoint :: PackageName -> PackageName -> ServerM Text
packageCabalFileEndpoint pname1 pname2 = do
  -- For legacy reasons, this path requires both package names to be the same
  unless (pname1 == pname2) $ throwError err404
  liftDB $ doSelect1 $ do
    rev <- getLatestVersionAndRev $ lit pname1
    pure $ metadataCabalFile rev


--------------------------------------------------------------------------------
-- /package/:package/preferred

data PreferredVersions = PreferredVersions
  { pv_packageName :: PackageName
  , getPreferredVersions :: Map Version VersionStatus
  }
  deriving stock (Eq, Ord, Show, Generic)

instance Arbitrary PreferredVersions where
  arbitrary = PreferredVersions <$> arbitrary <*> arbitrary

instance ToJSON PreferredVersions where
  toJSON (PreferredVersions _ vs) = do
    let (normal, deprecated) = partition ((== Normal) . snd) $ M.toList vs
    object
      [ "normal-version" .= fmap (Pretty.prettyShow . fst) normal
      , "deprecated-version" .= fmap (Pretty.prettyShow . fst) deprecated
      ]

instance ToObject PreferredVersions where
  toObject (PreferredVersions pkg vs) =
      [ "package" .= unPackageName pkg
      , "versions" .= M.mapKeys Pretty.prettyShow vs
      ]

instance HasTemplate HTML PreferredVersions where
  templateFor _ _ = "packages/preferred.html"

packagePreferredVersionsEndpoint :: PackageName -> ServerM PreferredVersions
packagePreferredVersionsEndpoint pname = do
  versions <- liftDB $ doSelect $ do
    pkg <- each packageNameSchema
    where_ $ packageName pkg ==. lit pname
    pkgv <- each pkgInfoSchema
    where_ $ pkgId pkgv ==. packageNameId pkg
    pure (packageVersion pkgv, pkgInfoDeprecated pkgv)
  pure $ PreferredVersions pname $ M.fromList $ fmap (fmap $ bool Normal Deprecated) versions


--------------------------------------------------------------------------------
-- /package/:package/uploader

packageMirrorUploader :: Either PackageName PackageIdentifier -> ServerM UserName
packageMirrorUploader pname =
  liftDB $ doSelect1 $ do
    pkgv <- either (getLatestVersionAndRev . lit) getLatestRev pname
    u <- each usersSchema
    where_ $ metadataUploader pkgv ==. userId u
    pure $ userName u


--------------------------------------------------------------------------------
-- /package/:package/upload-time

packageMirrorUploadTime :: Either PackageName PackageIdentifier -> ServerM UTCTime
packageMirrorUploadTime pname =
  liftDB $ doSelect1 $ do
    pkgv <- either (getLatestVersionAndRev . lit) getLatestRev pname
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
    :: Either PackageName PackageIdentifier
    -> PackageIdentifier
    -> ServerM BL.ByteString
packageTarball epname tarball = do
  -- We want to ensure that the tarball name lines up with the package we were
  -- given. If we have only a 'PackageName', then ensure it's the same package
  -- name as on the 'PackageIdentifier'.
  --
  -- If we have a full package identifier, then make sure they agree!
  case epname of
    Left pname | pname == pkgName tarball -> pure ()
    Right pid | pid == tarball -> pure ()
    _ -> throwError err404

  mblob <-
    liftDB $ doSelect1 $ optional $ fmap tarballBlobGz $ getLatestTarball tarball
  case mblob of
    Just blob -> do
      store <- asks serverBlobStore
      liftIO $ Blob.get store blob
    Nothing -> throwError err404

