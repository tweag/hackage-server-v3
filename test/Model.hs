{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

module Model where

import Control.Monad (void)
import Control.Arrow ((&&&))
import Data.Bifunctor (first)
import Data.Data (Data)
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
import Hackage.Objects
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
  sshrink :: args -> a -> [a]
  sshrink _ _ = []


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

  shrink mh = filter validModelHackage $ mconcat
    [ do
        let users = mh_users mh
            userRefs = S.fromList $ fmap userToUserRef $ M.elems users
        packages <- sshrink userRefs $ mh_packages mh
        pure $ ModelHackage packages users
    , do
        users <- shrinkUsers $ mh_users mh
        let userRefs = S.fromList $ fmap userToUserRef $ M.elems users
        restrictedPackages <- restrictPackagesTo userRefs $ mh_packages mh
        packages <- restrictedPackages : sshrink userRefs restrictedPackages
        pure $ ModelHackage packages users
    ]


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

  sshrink us pkg =
    filter validModelPackage $ mconcat
      [ do
          versions <- sshrink us $ mp_versions pkg
          pure $ pkg { mp_versions = versions }
      , do
          deprecated <- shrink $ mp_deprecated pkg
          pure $ pkg { mp_deprecated = deprecated }
      ]

validModelPackage :: ModelPackage -> Bool
validModelPackage = not . null . mp_versions


shrinkUsers :: Map UserId ModelUser -> [Map UserId ModelUser]
shrinkUsers users =
  filter validUsers $ do
    uid <- M.keys users
    pure $ M.delete uid users


validUsers :: Map UserId ModelUser -> Bool
validUsers = not . null


restrictPackagesTo :: Set ModelUserRef -> Map PackageName ModelPackage -> [Map PackageName ModelPackage]
restrictPackagesTo us = traverse $ restrictPackageTo us


restrictPackageTo :: Set ModelUserRef -> ModelPackage -> [ModelPackage]
restrictPackageTo us pkg =
  filter validModelPackage $ do
    versions <- traverse (restrictPackageInfoTo us) $ mp_versions pkg
    pure $ pkg { mp_versions = versions }


restrictPackageInfoTo :: Set ModelUserRef -> ModelPkgInfo -> [ModelPkgInfo]
restrictPackageInfoTo us pkginfo =
  filter validModelPkgInfo $ do
    revisions <- traverse (restrictMetaRevTo us) $ mpi_revisions pkginfo
    pure $ pkginfo { mpi_revisions = revisions }


restrictMetaRevTo :: Set ModelUserRef -> ModelMetaRev -> [ModelMetaRev]
restrictMetaRevTo us metarev
  | S.member (mmr_user metarev) us = [metarev]
  | otherwise = do
      user <- S.toAscList us
      pure $ metarev { mmr_user = user }


-- | A model of a PkgInfo.
data ModelPkgInfo = ModelPkgInfo
  { mpi_revisions :: [ModelMetaRev]
  , mpi_deprecated :: Bool
  }
  deriving stock (Eq, Ord, Show, Generic, Data)

validModelPkgInfo :: ModelPkgInfo -> Bool
validModelPkgInfo = not . null . mpi_revisions

instance SomewhatArbitrary a b => SomewhatArbitrary a [b] where
  sarbitrary = listOf . sarbitrary
  sshrink args = shrinkList (sshrink args)

instance (Arbitrary x, SomewhatArbitrary a y) => SomewhatArbitrary a (x, y) where
  sarbitrary a = (,) <$> arbitrary <*> sarbitrary a
  sshrink a (x, y) =
    mconcat
      [ do
          x' <- shrink x
          pure (x', y)
      , do
          y' <- sshrink a y
          pure (x, y')
      ]

instance (Arbitrary k, Ord k, SomewhatArbitrary a v) => SomewhatArbitrary a (Map k v) where
  sarbitrary = fmap M.fromList . sarbitrary
  sshrink a = fmap M.fromList . sshrink a . M.toList

instance SomewhatArbitrary (Set ModelUserRef) ModelPkgInfo where
  sarbitrary us =
    ModelPkgInfo
      <$>
        ( do
            Positive (Small n) <- arbitrary
            vectorOf n $ sarbitrary us
        )
      <*> arbitrary

  sshrink us pkginfo =
    filter validModelPkgInfo $ mconcat
      [ do
          revisions <- sshrink us $ mpi_revisions pkginfo
          pure $ pkginfo { mpi_revisions = revisions }
      , do
          deprecated <- shrink $ mpi_deprecated pkginfo
          pure $ pkginfo { mpi_deprecated = deprecated }
      ]

data ModelUserRef = ModelUserRef
  { mur_id :: UserId
  , mur_name :: UserName
  }
  deriving stock (Eq, Ord, Show, Generic, Data)

instance SomewhatArbitrary (Set ModelUserRef) ModelUserRef where
  sarbitrary = elements . S.toList
  sshrink us user = takeWhile (< user) $ S.toList us


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

  sshrink us (ModelMetaRev user time) =
    mconcat
      [ do
          user' <- sshrink us user
          pure $ ModelMetaRev user' time
      , do
          time' <- shrink time
          pure $ ModelMetaRev user time'
      ]


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


genExistingPackageLocator :: ModelHackage -> Gen (PackageLocator, ModelPkgInfo)
genExistingPackageLocator mh = oneof
  [ fmap (first Specific) $ genExistingPackageId mh
  , do
      (pname, pkg) <- genExistingPackage mh
      pure (Latest pname, snd $ M.findMax $ mp_versions pkg)
  ]


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
  loadModelUsers $ M.toList $ mh_users mh
  void $ loadModelPackages $ M.toList $ mh_packages mh


-- | Import a 'ModelPackage' into the database.
loadModelPackages :: [(PackageName, ModelPackage)] -> ServerM [PkgId]
loadModelPackages pkgs = do
  pkgids <- liftDB $ doInsert $ Insert
    { into = packageNameSchema
    , rows = values $ do
        (pkgname, pkginfo) <- pkgs
        pure $ PackageNameRow
          { packageNameId = newPrimaryKey
          , packageName = lit pkgname
          , packageDeprecated = lit $ mp_deprecated pkginfo
          }
    , onConflict = Abort
    , returning = Returning packageNameId
    }
  loadModelPkgInfos $ do
    (pkgid, pkg) <- zip pkgids $ fmap snd pkgs
    (version, pkginfo) <- M.toList $ mp_versions pkg
    pure (pkgid, version, pkginfo)
  pure pkgids


-- | Import a 'ModelPkgInfo' into the database.
loadModelPkgInfos :: [(PkgId, Version, ModelPkgInfo)] -> ServerM [PkgInfoId]
loadModelPkgInfos versions  = do
  pkginfoids <- liftDB $ doInsert $ Insert
    { into = pkgInfoSchema
    , rows = values $ do
        (pkgid, version, pkginfo) <- versions
        pure $ PkgInfoRow
          { pkgInfoId = newPrimaryKey
          , pkgId = lit pkgid
          , packageVersion = lit version
          , pkgInfoDeprecated = lit $ mpi_deprecated pkginfo
          }
    , onConflict = Abort
    , returning = Returning pkgInfoId
    }
  _ <- loadModelMetaRevs $ do
    (pkginfoid, (_, _, pkginfo)) <- zip pkginfoids versions
    (revix, rev) <- zip [MetadataRevIx 0..] $ mpi_revisions pkginfo
    pure (pkginfoid, revix, rev)
  pure pkginfoids


loadModelMetaRevs :: [(PkgInfoId, MetadataRevIx, ModelMetaRev)] -> ServerM [PkgRevId]
loadModelMetaRevs revs = do
  liftDB $ doInsert $ Insert
    { into = metadataRevisionsSchema
    , rows = values $ do
        (pii, revix, rev) <- revs
        pure $ MetadataRevisionRow
            { metadataId = newPrimaryKey
            , metadataPkgId = lit pii
            , metadataRevId = lit revix
            , metadataTime = lit $ mmr_time rev
            , metadataUploader = lit $ mur_id $ mmr_user rev
            , metadataCabalFile = lit mempty
            }
    , onConflict = Abort
    , returning = Returning metadataId
    }


loadModelUsers :: [(UserId, ModelUser)] -> ServerM ()
loadModelUsers us =
  liftDB $ doInsert_ $ Insert
    { into = usersSchema
    , rows = values $ do
        (uid, user) <- us
        pure $
          UsersRow
            { userId = lit uid
            , userName = lit $ mu_name user
            , userStatus = lit $ mu_status user
            }
    , onConflict = Abort
    , returning = NoReturning
    }

