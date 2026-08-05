{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

module Model where

import Control.Arrow ((&&&))
import Data.Data (Data)
import Data.Foldable
import Data.Hashable
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Time (UTCTime)
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.Version
import GHC.Generics
import Hackage.Orphans ()
import Hackage.Schemas.Packages
import Hackage.Schemas.Users
import Hackage.ServerM
import Hackage.Types
import Hackage.Types.PrimaryKey
import Hackage.Utils
import Rel8 hiding (null, filter, and, listOf)
import Test.QuickCheck


-- | Like 'Arbitrary', but for types that require input in order to generate.
class SomewhatArbitrary args a | a -> args where
  sarbitrary :: args -> Gen a


-- | A model of a full Hackage database. This type is significantly easier to
-- generate than the corresponding schema row objects, so our tests generate
-- these and convert load them into the database. But we can write easy
-- properties against the model and verify that they hold over the real
-- implementation.
data ModelHackage = ModelHackage
  { mh_packages :: Map PackageName ModelPackage
  , mh_users :: Map UserId ModelUser
  }
  deriving stock (Eq, Ord, Show, Generic, Data)

instance Arbitrary ModelHackage where
  arbitrary = do
    -- Generate users, and use 'sarbitrary' to thread them through the
    -- remaining generators.
    users <- suchThat arbitrary (not . null)
    ModelHackage
      <$> suchThat (sarbitrary $ S.fromList $ fmap userToUserRef users) (not . null)
      <*> pure (M.fromList $ fmap (getUserId &&& id) users)

getUserId :: ModelUser -> UserId
getUserId = UserId . fromIntegral . abs . hash


validModelHackage :: ModelHackage -> Bool
validModelHackage mh = and
  [ not $ null $ mh_packages mh
  ]


-- | A model of a User.
data ModelUser = ModelUser
  { mu_name :: UserName
  , mu_status :: UserStatus
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass Hashable

instance Arbitrary ModelUser where
  arbitrary =
    ModelUser
      <$> arbitrary
      <*> arbitrary

userToUserRef :: ModelUser -> ModelUserRef
userToUserRef mu@(ModelUser name _) = ModelUserRef (getUserId mu) name


-- | A model of a package.
data ModelPackage = ModelPackage
  { mp_versions :: Map Version ModelPkgInfo
  , mp_deprecated :: Bool
  }
  deriving stock (Eq, Ord, Show, Generic, Data)

instance SomewhatArbitrary (Set ModelUserRef) ModelPackage where
  sarbitrary us =
    ModelPackage
      <$> scale (`div` 2) (suchThat (sarbitrary us) (not . null))
      <*> arbitrary

validModelPackage :: ModelPackage -> Bool
validModelPackage = not . null . mp_versions


-- | A model of a PkgInfo.
data ModelPkgInfo = ModelPkgInfo
  { mpi_revisions :: [ModelMetaRev]
  , mpi_deprecated :: Bool
  }
  deriving stock (Eq, Ord, Show, Generic, Data)

instance SomewhatArbitrary a b => SomewhatArbitrary a [b] where
  sarbitrary = listOf . sarbitrary

instance (Arbitrary x, SomewhatArbitrary a y) => SomewhatArbitrary a (x, y) where
  sarbitrary a = (,) <$> arbitrary <*> sarbitrary a

instance (Arbitrary k, Ord k, SomewhatArbitrary a v) => SomewhatArbitrary a (Map k v) where
  sarbitrary = fmap M.fromList . sarbitrary

instance SomewhatArbitrary (Set ModelUserRef) ModelPkgInfo where
  sarbitrary us =
    ModelPkgInfo
      <$>
        ( do
            Positive (Small n) <- arbitrary
            vectorOf n $ sarbitrary us
        )
      <*> arbitrary

data ModelUserRef = ModelUserRef
  { mur_id :: UserId
  , mur_name :: UserName
  }
  deriving stock (Eq, Ord, Show, Generic, Data)


instance SomewhatArbitrary (Set ModelUserRef) ModelUserRef where
  sarbitrary = elements . S.toList

-- | A model of a package metadata revision.
data ModelMetaRev = ModelMetaRev
  { mmr_user :: ModelUserRef
  , mmr_time :: UTCTime
  }
  deriving stock (Eq, Ord, Show, Generic, Data)

instance SomewhatArbitrary (Set ModelUserRef) ModelMetaRev where
  sarbitrary us =
    ModelMetaRev
      <$> sarbitrary us
      <*> arbitrary


-- | Find a 'ModelPackage' inside of 'ModelHackage'.
lookupPackage :: ModelHackage -> PackageName -> Maybe ModelPackage
lookupPackage mh pkg = do
  M.lookup pkg $ mh_packages mh


-- | Find a 'ModelPkgInfo' inside of 'ModelHackage'.
lookupPackageInfo :: ModelHackage -> PackageId -> Maybe ModelPkgInfo
lookupPackageInfo mh (PackageIdentifier pkg v) = do
  mp <- M.lookup pkg $ mh_packages mh
  M.lookup v $ mp_versions mp


-- | Helper function for implementing genExistingX.
genExisting :: Ord k => (a -> Map k v) -> a -> Gen (k, v)
genExisting f a = do
  let fa = f a
  k <- elements $ M.keys fa
  pure (k, fa M.! k)


-- | Get an arbitrary 'PackageName' that is guaranteed to exist in the model.
genExistingUser :: ModelHackage -> Gen (UserId, ModelUser)
genExistingUser = genExisting mh_users


-- | Get an arbitrary 'PackageName' that is guaranteed to exist in the model.
genExistingPackage :: ModelHackage -> Gen (PackageName, ModelPackage)
genExistingPackage = genExisting mh_packages


-- | Get an arbitrary 'Version' that is guaranteed to exist in the model.
genExistingVersion :: ModelPackage -> Gen (Version, ModelPkgInfo)
genExistingVersion = genExisting mp_versions


-- | Get an arbitrary 'PackageIdentifier' that is guaranteed to exist in the
-- model.
genExistingPackageId :: ModelHackage -> Gen (PackageIdentifier, ModelPkgInfo)
genExistingPackageId mh = do
  (pkgname, mp) <- genExistingPackage mh
  (version, pkginfo) <- genExistingVersion mp
  pure (PackageIdentifier pkgname version, pkginfo)


-- | Import a 'ModelHackage' into the database.
loadModelHackage :: ModelHackage -> ServerM ()
loadModelHackage mh = do
  for_ (M.toList $ mh_users mh) $ uncurry loadModelUser
  for_ (M.toList $ mh_packages mh) $ uncurry loadModelPackage


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


loadModelUser :: UserId -> ModelUser -> ServerM ()
loadModelUser uid user =
  liftDB $ doInsert_ $ Insert
    { into = usersSchema
    , rows = values
        [ UsersRow
            { userId = lit uid
            , userName = lit $ mu_name user
            , userStatus = lit $ mu_status user
            }
        ]
    , onConflict = Abort
    , returning = NoReturning
    }

