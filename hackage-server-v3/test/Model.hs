{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PackageImports         #-}
{-# LANGUAGE UndecidableInstances   #-}

module Model where

import Control.Lens ((&), (.~), (%~))
import "hackage-server-v3" Data.TarIndex
import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Control.Arrow ((&&&))
import Control.Monad (void, guard)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Bifunctor (first)
import Data.BlobStorage qualified as Blob
import Data.ByteString (StrictByteString, fromStrict)
import Data.ByteString.Char8 qualified as BS8
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Hashable
import Data.List (inits)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Traversable
import Data.Generics.Labels ()
import Distribution.Pretty qualified as Pretty
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.Version
import Distribution.Utils.MD5 (md5)
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
import Servant.Tarball
import System.FilePath ((</>))
import Test.QuickCheck
import Unsafe.Coerce (unsafeCoerce)


-- | Generating a blob store is very slow, so we provide a means of toggling
-- whether a given test actually needs it.
data WantsBlobStore = WithBlobStore | NoBlobStore
  deriving stock (Eq, Ord, Show)


genByteString :: Gen StrictByteString
genByteString = fmap (BS8.pack . getASCIIString) arbitrary


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
      <$> genSmallMap (sarbitrary $ S.fromList $ fmap userToUserRef users)
      <*> pure (M.fromList $ fmap (getUserId &&& id) users)

  shrink mh = filter validModelHackage $ mconcat
    [ do
        guard $ length (mh_users mh) > 1
        uid <- M.keys $ mh_users mh
        let users' = M.delete uid $ mh_users mh
        pure $ ModelHackage (deleteOwnedBy uid $ mh_packages mh) users'
    , do
        guard $ length (mh_users mh) == 1
        let users = mh_users mh
            userRefs = S.fromList $ fmap userToUserRef $ M.elems users
        packages <- sshrink userRefs $ mh_packages mh
        pure $ ModelHackage packages users
    ]

deleteOwnedBy :: UserId -> Map PackageName ModelPackage -> Map PackageName ModelPackage
deleteOwnedBy uid pkgs =
  M.fromList $ do
    (pname, pkg) <- M.toList pkgs
    let pkg' =
          pkg & #mp_versions .~ M.fromList do
            (v, pkginfo) <- M.toList $ mp_versions pkg
            let pkginfo' = pkginfo & #mpi_revisions %~ filter ((/= uid) . mur_id . mmr_user)
            guard $ not $ null $ mpi_revisions pkginfo'
            pure (v, pkginfo')
    guard $ not $ null $ mp_versions pkg'
    pure (pname, pkg')



getUserId :: ModelUser -> UserId
getUserId = UserId . fromIntegral . abs . hash


validModelHackage :: ModelHackage -> Bool
validModelHackage mh = and
  [ not $ null $ mh_packages mh
  , not $ null $ mh_users mh
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


-- | Generate a small map, by giving a generator for its assocs. We use this
-- rather than the standard 'Arbitrary' instance, since we want to bound the
-- amount of data we generate.
genSmallMap :: Ord k => Gen (k, v) -> Gen (Map k v)
genSmallMap gen = fmap M.fromList $ do
  n <- chooseInt (1, 10)
  vectorOf n gen


instance SomewhatArbitrary (Set ModelUserRef) ModelPackage where
  sarbitrary us =
    ModelPackage
      <$> genSmallMap (sarbitrary us)
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


-- | A model of a PkgInfo.
data ModelPkgInfo = ModelPkgInfo
  { mpi_revisions :: [ModelMetaRev]
  , mpi_deprecated :: Bool
  , mpi_source :: ModelTarball
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
            n <- chooseInt (1, 10)
            vectorOf n $ sarbitrary us
        )
      <*> arbitrary
      <*> arbitrary

  sshrink us pkginfo =
    filter validModelPkgInfo $ mconcat
      [ do
          revisions <- sshrink us $ mpi_revisions pkginfo
          pure $ pkginfo { mpi_revisions = revisions }
      , do
          deprecated <- shrink $ mpi_deprecated pkginfo
          pure $ pkginfo { mpi_deprecated = deprecated }
      , do
          source <- shrink $ mpi_source pkginfo
          pure $ pkginfo { mpi_source = source }
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
  , mmr_cabal :: StrictByteString
  }
  deriving stock (Eq, Ord, Show, Generic, Data)

instance SomewhatArbitrary (Set ModelUserRef) ModelMetaRev where
  sarbitrary us =
    ModelMetaRev
      <$> sarbitrary us
      <*> arbitrary
      <*> genByteString

  sshrink us (ModelMetaRev user time cabal) =
    mconcat
      [ do
          user' <- sshrink us user
          pure $ ModelMetaRev user' time cabal
      , do
          time' <- shrink time
          pure $ ModelMetaRev user time' cabal
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
loadModelHackage :: WantsBlobStore -> ModelHackage -> ServerM ()
loadModelHackage wantsBs mh = do
  loadModelUsers $ M.toList $ mh_users mh
  void $ loadModelPackages wantsBs $ M.toList $ mh_packages mh


-- | Import a 'ModelPackage' into the database.
loadModelPackages :: WantsBlobStore -> [(PackageName, ModelPackage)] -> ServerM [PkgId]
loadModelPackages wantsBs pkgs = do
  pkgids <- runDB $ doInsert $ Insert
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
  _ <- loadModelPkgInfos wantsBs $ do
    (pkgid, (pkgname, pkg)) <- zip pkgids pkgs
    (version, pkginfo) <- M.toList $ mp_versions pkg
    pure (pkgid, PackageIdentifier pkgname version, pkginfo)
  pure pkgids


-- | Import a 'ModelPkgInfo' into the database.
loadModelPkgInfos :: WantsBlobStore -> [(PkgId, PackageId, ModelPkgInfo)] -> ServerM [PkgInfoId]
loadModelPkgInfos wantsBs versions = do
  pkginfoids <- runDB $ doInsert $ Insert
    { into = pkgInfoSchema
    , rows = values $ do
        (pkgid, PackageIdentifier _ version, pkginfo) <- versions
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

  -- For now we cheat and just assume there is a single tarball revision.
  blobs <- for (zip pkginfoids versions) $ \(pkginfoid, (_, pkgid, pkg)) -> do
    blobid <-
      case wantsBs of
        NoBlobStore -> pure $ BlobId $ md5 ""
        WithBlobStore ->
          loadTarball (Pretty.prettyShow pkgid) $ mpi_source pkg
    pure (pkginfoid, blobid, head $ mpi_revisions pkg)
  _ <- runDB $ doInsert $ Insert
    { into = packageTarballRevisionsSchema
    , rows = values $ do
       (pkginfoid, blobid, rev) <- blobs
       pure $ TarballRevisionRow
        { tarballRevId = newPrimaryKey
        , tarballPkgId = lit pkginfoid
        , tarballRevIx = lit 0
        , tarballTime = lit $ mmr_time rev
        , tarballUploader = lit $ mur_id $ mmr_user rev
        , tarballBlobNoGz = lit blobid
        , tarballBlobGz =
            -- Safe, except that we'll serve it with the wrong mimetype.
            lit $ unsafeCoerce blobid
        , tarballGzLength = lit 0 -- Stupid default, but I don't think it's actually used?
        , tarballGzHash = lit mempty -- Stupid default, but I don't think it's actually used?
        }
    , onConflict = Abort
    , returning = Returning tarballRevId
    }

  pure pkginfoids


loadModelMetaRevs :: [(PkgInfoId, MetadataRevIx, ModelMetaRev)] -> ServerM [PkgRevId]
loadModelMetaRevs revs = do
  runDB $ doInsert $ Insert
    { into = metadataRevisionsSchema
    , rows = values $ do
        (pii, revix, rev) <- revs
        pure $ MetadataRevisionRow
            { metadataId = newPrimaryKey
            , metadataPkgId = lit pii
            , metadataRevId = lit revix
            , metadataTime = lit $ mmr_time rev
            , metadataUploader = lit $ mur_id $ mmr_user rev
            , metadataCabalFile = lit $ mmr_cabal rev
            }
    , onConflict = Abort
    , returning = Returning metadataId
    }


loadModelUsers :: [(UserId, ModelUser)] -> ServerM ()
loadModelUsers us =
  runDB $ doInsert_ $ Insert
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


data ModelTarball = ModelTarball
  { mt_filesystem :: Map PathSeg FileEntry
  }
  deriving stock (Eq, Ord, Show, Generic, Data)

instance Arbitrary ModelTarball where
  arbitrary = fmap ModelTarball $ genSmallMap arbitrary
  shrink = fmap (ModelTarball . M.fromList) . init . drop 1 . inits . M.toList . mt_filesystem

newtype PathSeg = PathSeg { getPathSeg :: FilePath }
  deriving newtype (Eq, Ord, Show)
  deriving stock (Data)


instance Arbitrary PathSeg where
  arbitrary = do
    n <- chooseInt (1, 20)
    fmap PathSeg $ vectorOf n $ elements $ mconcat
      [ ['a' .. 'z']
      , ['A' .. 'Z']
      , ['0' .. '9']
      , ".-"
      ]
  shrink = coerce . init . drop 1 . inits . getPathSeg

instance Arbitrary BS8.ByteString where
  arbitrary = genByteString

data FileEntry
  = File StrictByteString
  | Dir (Map PathSeg FileEntry)
  deriving stock (Eq, Ord, Show, Generic, Data)

instance Arbitrary FileEntry where
  arbitrary = sized $ \n ->
    case n <= 1 of
      True -> fmap File genByteString
      False -> oneof
        [ fmap Dir $ genSmallMap $ scale (`div` 10) arbitrary
        , fmap File genByteString
        ]
  shrink = genericShrink


loadTarball :: FilePath -> ModelTarball -> ServerM (BlobId Tarball)
loadTarball dir (ModelTarball fs) = do
  store <- asks serverBlobStore
  let es = flattenFs dir fs
  x <- liftIO $ Blob.addLazy store $ Tar.write es
  let blobid = unsafeCoerce x
  _ <- loadTarIndices blobid es
  pure blobid


getPaths :: Map PathSeg FileEntry -> [([Text], StrictByteString)]
getPaths fs = do
  (PathSeg seg, c) <- M.toList fs
  let segt = T.pack seg
  case c of
    Dir fs' -> fmap (first (segt :)) $ getPaths fs'
    File contents -> pure ([segt], contents)


flattenFs :: FilePath -> Map PathSeg FileEntry -> [Tar.Entry]
flattenFs dir fs = do
  (PathSeg seg, c) <- M.toList fs
  let path = dir </> seg
  case c of
    Dir fs' -> do
      Tar.directoryEntry (either error id $ Tar.toTarPath True path) : flattenFs path fs'
    File content ->
      pure $
        Tar.fileEntry
          (either error id $ Tar.toTarPath False path)
          (fromStrict content)


loadTarIndices :: BlobId Tarball -> [Tar.Entry] -> ServerM [TarIndexId]
loadTarIndices bid es = do
  let Right m = construct $ makeEntries es
  runDB $ doInsert $ Insert
      { into = tarIndexSchema
      , rows = do
          (path, off) <- values $ do
            (k, v) <- M.toList m
            pure $ lit (T.pack k, v)
          pure $ TarIndexRow
            { tarIndexId = newPrimaryKey
            , tarIndexBlob = lit bid
            , tarIndexPath = path
            , tarIndexOffset = off
            }
      , onConflict = DoNothing
      , returning = Returning tarIndexId
      }



makeEntries :: [Tar.Entry] -> Tar.Entries ()
makeEntries = Tar.unfoldEntries $ \case
  [] -> Right Nothing
  (a : as) -> Right $ Just (a, as)

