{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Hackage.API.Type where

import Data.Aeson hiding (Result(..))
import Data.ByteString (StrictByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.List (partition, sortOn)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Ord (Down(..))
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Arbitrary ()
import Data.Time (UTCTime)
import Data.Trie
import Distribution.Pretty qualified as Pretty
import Distribution.SPDX.License (License)
import Distribution.Types.Dependency as Cabal
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import GHC.Generics (Generic)
import Hackage.Objects
import Hackage.Types
import Servant.API
import Servant.EDE
import Servant.HackageCombinators.CaptureExt
import Servant.HackageCombinators.DynamicGet
import Servant.HackageCombinators.NegotiableContent
import Servant.HackageCombinators.PermanentRedirect
import Servant.HackageCombinators.UserDomain
import Servant.Tarball
import Test.QuickCheck


data PackageDbApi mode = PackageDbApi
  { -- | This route only exists to redirect the legacy
    -- @package/:package/revisions/.:format@ over to its new home at
    -- 'pkgdb_api_revisions', since the former is a very strange route for
    -- an API.
    pkgdb_api_revisions_redirect :: mode
      :- NegotiableContent
      :> "package"
      :> Capture "package" PackageLocator
      :> "revisions"
      :> ""
      :> PermanentRedirect
  , pkgdb_api_revisions :: mode
      :- NegotiableContent
      :> "package"
      :> Capture "package" PackageLocator
      :> "revisions"
      :> Get '[HTML, JSON] (WithPackage Revisions)
  , pkgdb_api_tarball :: mode
      :- "package"
      :> Capture "package" PackageLocator
      :> CaptureExt "tarball" PackageIdentifier "tar.gz"
      :> Get '[Tarball] BSL.ByteString
  , pkgdb_api_distroMonitor :: mode
      :- NegotiableContent
      :> "package"
      :> Capture "package" PackageName
      :> "distro-monitor"
      :> Get '[HTML] (WithPackageName AllTarballs)
  , pkgdb_api_uploader :: mode
      :- "package"
      :> Capture "package" PackageLocator
      :> "uploader"
      :> Get '[PlainText] UserName
  , pkgdb_api_uploadTime :: mode
      :- "package"
      :> Capture "package" PackageLocator
      :> "upload-time"
      :> Get '[PlainText] UTCTime
  , pkgdb_api_dependencies :: mode
      :- "package"
      :> Capture "package" PackageLocator
      :> "dependencies"
      :> Get '[HTML] (WithPackage Dependencies)
  , pkgdb_api_versions :: mode
      :- "package"
      :> CaptureExt "package" PackageName "json"
      :> Get '[JSON] PackageVersions
  , pkgdb_api_metadata :: mode
      :- "package"
      :> CaptureExt "package" PackageIdentifier "json"
      :> Get '[JSON] PackageBasicDescriptionDTO
  , pkgdb_api_cabalFile :: mode
      :- "package"
      :> Capture "package" PackageName
      :> CaptureExt "package" PackageName "cabal"
      :> Get '[PlainText] StrictByteString
  , pkgdb_api_preferredVersions :: mode
      :- NegotiableContent
      :> "package"
      :> Capture "package" PackageName
      :> "preferred"
      :> Get '[HTML, JSON] (WithPackageName PreferredVersions)
  , pkgdb_api_tarballContent :: mode
      :- UserDomain
      :> "package"
      :> Capture "package" PackageLocator
      :> "src"
      :> CaptureAll "src" Text
      :> DynamicGet
           '[ '(PlainText, Text)
            , '(HTML, DirectoryListing)
            ]
  }
  deriving stock (Generic)


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


--------------------------------------------------------------------------------
-- /package/:packageid.json

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
    let (normal, deprecated) = partition ((== Normal) . snd) $ sortOn (Down . fst) $ M.toList vs
    object $
      [ "normal-version" .= fmap (Pretty.prettyShow . fst) normal
        | not $ null normal
        ] <>
      [ "deprecated-version" .= fmap (Pretty.prettyShow . fst) deprecated
        | not $ null deprecated
        ]


--------------------------------------------------------------------------------
-- /package/:package/distro-monitor[.html]

newtype AllTarballs = AllTarballs
  { allTarballs :: [PackageIdentifier]
  }
  deriving stock (Eq, Ord, Show)

instance Arbitrary AllTarballs where
  arbitrary = AllTarballs <$> arbitrary


--------------------------------------------------------------------------------
-- /package/:package/dependencies


data Dependencies = Dependencies
  { isCandidate :: Bool
  , dependencies :: [Cabal.Dependency]
  }
  deriving stock (Eq, Ord, Show)

instance Arbitrary Dependencies where
  arbitrary = Dependencies <$> arbitrary <*> arbitrary


--------------------------------------------------------------------------------
-- /package/:package/revisions

newtype Revisions = Revisions
  { _unRevisions :: [Revision]
  }
  deriving newtype (Eq, Ord, Show, Arbitrary)


data Revision = Revision
  { number :: MetadataRevIx
  , sha256 :: Text
  , time :: UTCTime
  , user :: UserName
  }
  deriving stock (Eq, Ord, Show)

instance Arbitrary Revision where
  arbitrary = Revision
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance ToJSON Revisions where
  toJSON (Revisions revs) = toJSON revs

instance ToJSON Revision where
  toJSON rev = object
    [ "number" .= number rev
    , "sha256" .= sha256 rev
    , "time"   .= time rev
    , "user"   .= user rev
    ]


--------------------------------------------------------------------------------
-- /package/:package/src/...

newtype DirectoryListing = DirectoryListing (Trie Text)
  deriving newtype (Eq, Ord, Show, Arbitrary)

