{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -Wno-orphans       #-}


module Hackage.Objects where

import Data.Proxy (Proxy(..))
import Data.Aeson hiding (Result(..))
import Data.Profunctor
import Data.Schema qualified as S
import Data.Text qualified as T
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.Version
import GHC.Generics (Generic)
import Hackage.Orphans ()
import Servant.API
import Servant.EDE
import Text.EDE.Filters (Quote, Unquote)
import Test.QuickCheck (Arbitrary(..))


-- | A 'PackageLocator' helps us differentiate between the packages @foo@ and
-- @foo-1.0.0.0@, where in the first case we'd like to implicitly expand out
-- the version to be the latest version.
--
-- In Hackage V2, the first case is represented as @foo-@ (with an empty
-- version), but not all routes support this implicit lookup. Since routing in
-- V3 is type-directed, we use a 'PackageLocator' as a single type that
-- represents both of these cases. We reserve 'PackageName' for cases in which
-- we need to talk explicitly about a package /without a version/, and
-- 'PackageIdentifier' for cases in which we have a specific version in mind.
data PackageLocator
  = Latest PackageName
  | Specific PackageIdentifier
  deriving stock (Eq, Ord, Show, Generic)

instance FromHttpApiData PackageLocator where
  parseUrlPiece x =
    case parseUrlPiece x of
      Right pkg -> pure $ Specific pkg
      Left _ -> fmap Latest $ parseUrlPiece x

instance ToHttpApiData PackageLocator where
  toUrlPiece (Latest pkg) = toUrlPiece pkg
  toUrlPiece (Specific pkg) = toUrlPiece pkg

--------------------------------------------------------------------------------

-- | Helper structure for automatically attaching package metadata to the HTML
-- templating engine.
data WithPackage a = WithPackage
  { package :: PackageIdentifier
  , value :: a
  }
  deriving stock (Eq, Ord, Show, Generic)

instance Arbitrary a => Arbitrary (WithPackage a) where
  arbitrary = WithPackage <$> arbitrary <*> arbitrary

instance ToObject a => ToObject (WithPackage a) where
  toObject (WithPackage pkg v) = ["package" .= pkg] <> toObject v

instance ToJSON a => ToJSON (WithPackage a) where
  toJSON (WithPackage _ v) = toJSON v

instance HasTemplate c a => HasTemplate c (WithPackage a) where
  templateFor c _ = templateFor c $ Proxy @a


--------------------------------------------------------------------------------

-- | Helper structure for automatically attaching package metadata to the HTML
-- templating engine.
data WithPackageName a = WithPackageName
  { package :: PackageName
  , value :: a
  }
  deriving stock (Eq, Ord, Show, Generic)

instance Arbitrary a => Arbitrary (WithPackageName a) where
  arbitrary = WithPackageName <$> arbitrary <*> arbitrary

instance ToObject a => ToObject (WithPackageName a) where
  toObject (WithPackageName pkg v) = ["package" .= pkg] <> toObject v

instance ToJSON a => ToJSON (WithPackageName a) where
  toJSON (WithPackageName _ v) = toJSON v

instance HasTemplate c a => HasTemplate c (WithPackageName a) where
  templateFor c _ = templateFor c $ Proxy @a


--------------------------------------------------------------------------------

instance S.ToSchema PackageIdentifier where
  schema = S.object $
    PackageIdentifier
      <$> pkgName S..= S.field "name" S.schema
      <*> pkgVersion S..= S.field "version" S.schema

deriving via Schema PackageIdentifier instance ToJSON PackageIdentifier
deriving via Schema PackageIdentifier instance FromJSON PackageIdentifier
deriving via Schema PackageIdentifier instance Quote PackageIdentifier
deriving via Schema PackageIdentifier instance Unquote PackageIdentifier

instance S.ToSchema PackageName where
  schema
    = dimap (T.pack . unPackageName) (mkPackageName . T.unpack)
    $ S.text "PackageName"

deriving via Schema PackageName instance ToJSON PackageName
deriving via Schema PackageName instance FromJSON PackageName
deriving via Schema PackageName instance Quote PackageName
deriving via Schema PackageName instance Unquote PackageName

instance S.ToSchema Version where
  schema
    = dimap versionNumbers mkVersion
    $ S.named "version"
    $ S.array S.schema

deriving via Schema Version instance ToJSON Version
deriving via Schema Version instance FromJSON Version
deriving via Schema Version instance Quote Version
deriving via Schema Version instance Unquote Version


--------------------------------------------------------------------------------

-- | Helper type for using @deriving via@ to instantiate common typeclass
-- instances via a 'S.ToSchema' instance.
newtype Schema a = ViaSchema { unViaSchema :: a }

instance S.ToSchema a => ToJSON (Schema a) where
  toJSON = S.schemaToJSON . unViaSchema

instance S.ToSchema a => FromJSON (Schema a) where
  parseJSON = fmap ViaSchema . S.schemaParseJSON

instance S.ToSchema a => Quote (Schema a)
instance S.ToSchema a => Unquote (Schema a)

