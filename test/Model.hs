module Model where

import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as M
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.Version
import GHC.Generics
import Hackage.Orphans ()
import Hackage.Schemas.Packages
import Hackage.ServerM
import Hackage.Types.PrimaryKey
import Hackage.Utils
import Rel8 hiding (null, filter)
import Test.QuickCheck


-- | A model of a full Hackage database. This type is significantly easier to
-- generate than the corresponding schema row objects, so our tests generate
-- these and convert load them into the database. But we can write easy
-- properties against the model and verify that they hold over the real
-- implementation.
data ModelHackage = ModelHackage
  { mh_packages :: Map PackageName ModelPackage
  }
  deriving stock (Eq, Ord, Show, Generic)

instance Arbitrary ModelHackage where
  arbitrary =
    ModelHackage
      <$> suchThat arbitrary (not . null)
  shrink = filter validModelHackage . genericShrink

validModelHackage :: ModelHackage -> Bool
validModelHackage = not . null . mh_packages


-- | A model of a package.
data ModelPackage = ModelPackage
  { mp_versions :: Map Version ModelPkgInfo
  , mp_deprecated :: Bool
  }
  deriving stock (Eq, Ord, Show, Generic)

instance Arbitrary ModelPackage where
  arbitrary =
    ModelPackage
      <$> scale (`div` 2) (suchThat arbitrary (not . null))
      <*> arbitrary
  shrink = filter validModelPackage . genericShrink

validModelPackage :: ModelPackage -> Bool
validModelPackage = not . null . mp_versions


-- | A model of a PkgInfo.
data ModelPkgInfo = ModelPkgInfo
  { mpi_deprecated :: Bool
  }
  deriving stock (Eq, Ord, Show, Generic)

instance Arbitrary ModelPkgInfo where
  arbitrary = ModelPkgInfo <$> arbitrary
  shrink = genericShrink


-- | Find a 'ModelPackage' inside of 'ModelHackage'.
lookupPackage :: ModelHackage -> PackageName -> Maybe ModelPackage
lookupPackage mh pkg = do
  M.lookup pkg $ mh_packages mh


-- | Find a 'ModelPkgInfo' inside of 'ModelHackage'.
lookupPackageInfo :: ModelHackage -> PackageId -> Maybe ModelPkgInfo
lookupPackageInfo mh (PackageIdentifier pkg v) = do
  mp <- M.lookup pkg $ mh_packages mh
  M.lookup v $ mp_versions mp


-- | Get an arbitrary 'PackageName' that is guaranteed to exist in the model.
genExistingPackage :: ModelHackage -> Gen (PackageName)
genExistingPackage = fmap pkgName . genExistingPackageId


-- | Get an arbitrary 'PackageIdentifier' that is guaranteed to exist in the
-- model.
genExistingPackageId :: ModelHackage -> Gen PackageIdentifier
genExistingPackageId mh = do
  pkgname <- elements $ M.keys $ mh_packages mh
  let mp = mh_packages mh M.! pkgname
  version <- elements $ M.keys $ mp_versions mp
  pure $ PackageIdentifier pkgname version


-- | Import a 'ModelHackage' into the database.
loadModelHackage :: ModelHackage -> ServerM ()
loadModelHackage (ModelHackage pkgs) = do
  for_ (M.toList pkgs) $ uncurry loadModelPackage


-- | Import a 'ModelPackage' into the database.
loadModelPackage :: PackageName -> ModelPackage -> ServerM PkgId
loadModelPackage pkgname pkginfo = do
  pkgid <- liftDB $ doInsert1 $ Insert
    { into = packageNameSchema
    , rows = values
        [ PackageNameRow
            { packageNameId = newPrimaryKey
            , packageName = lit pkgname
            , packageDeprecated = lit $ mp_deprecated pkginfo
            }
        ]
    , onConflict = Abort
    , returning = Returning packageNameId
    }
  for_ (M.toList $ mp_versions pkginfo) $ uncurry $ loadModelPkgInfo pkgid
  pure pkgid


-- | Import a 'ModelPkgInfo' into the database.
loadModelPkgInfo :: PkgId -> Version -> ModelPkgInfo -> ServerM PkgInfoId
loadModelPkgInfo pkgid version pkginfo = do
  pkginfoid <- liftDB $ doInsert1 $ Insert
    { into = pkgInfoSchema
    , rows = values
        [ PkgInfoRow
            { pkgInfoId = newPrimaryKey
            , pkgId = lit pkgid
            , packageVersion = lit version
            , pkgInfoDeprecated = lit $ mpi_deprecated pkginfo
            }
        ]
    , onConflict = Abort
    , returning = Returning pkgInfoId
    }
  pure pkginfoid

