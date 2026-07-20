{-# LANGUAGE OverloadedStrings #-}

module Mirror where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Control.Monad (when)
import Control.Monad.State
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Coerce
import Data.Foldable
import Data.Int
import Data.Map.Monoidal (MonoidalMap)
import Data.Map.Monoidal qualified as MM
import Data.Monoid(Sum(..))
import Data.Text qualified as T
import Data.Time.Clock.POSIX
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.Version (mkVersion)
import GHC.Generics
import Hackage.Schemas.Packages (PkgRevId, TarballRevisionRow(..), packageTarballRevisionsSchema)
import Hackage.Types
import Hasql.Session (statement, run)
import Import
import Rel8 hiding (run)
import Rel8 qualified as Rel8
import TestAPI (mkConn)

import Data.Acid (openLocalStateFrom, query, closeAcidState)
import Distribution.Server.Features.Core.State (initialPackagesState, GetPackagesState(..), PackagesState(..))
import Distribution.Server.Framework.BlobStorage qualified as Blob
import Distribution.Server.Packages.PackageIndex (PackageIndex(..))
import Hackage.TarBalls (insertTarEntries)
import System.FilePath



data RevState = RevState
  { rs_meta_rev :: Sum Int64
  , rs_tar_rev :: Sum Int64
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Semigroup, Monoid) via Generically RevState

metaRev :: RevState -> MetadataRevIx
metaRev = coerce . rs_meta_rev

tarRev :: RevState -> TarballRevIx
tarRev = coerce . rs_tar_rev

newMetaRev
    :: PackageIdentifier
    -> MetadataRevIx
    -> Tar.GenEntry BSL.ByteString b c
    -> SqlM (Query (Expr PkgRevId))
newMetaRev pid rev e = do
  pkgid <- mkPkgIdentifier pid
  mkMetadataRev
    pkgid
    rev
    (case Tar.entryContent e of
       Tar.NormalFile x _ -> BS.toStrict x
       _ -> error "Found something in the Tar that isn't a file"
    )
    ( posixSecondsToUTCTime $ fromIntegral $ Tar.entryTime e
    , UserId $ fromIntegral $ Tar.ownerId $ Tar.entryOwnership e
    )




data UpdateType = TarballRev | MetadataRev
  deriving stock (Eq, Ord, Show)

importIndexTar :: IO ()
importIndexTar = mkConn $ \conn -> do
  bs <- BSL.readFile "/home/sandy/01-index.tar"
  let es = Tar.read bs
  flip evalStateT (mempty @(MonoidalMap PackageIdentifier RevState)) $
    Tar.foldEntries (\e m -> do
      -- let o = Tar.entryOwnership e
      let ps = T.split (== '/') $ T.pack $ Tar.entryPath e
      when (ps !! 1 /= "preferred-versions") $ do
        let pname = ps !! 0
            pvers = mkVersion $ fmap (read . T.unpack) $ T.split (== '.') $ ps !! 1
            revtype =
              case ps !! 2 of
                "package.json" -> TarballRev
                _ -> MetadataRev
            pid = PackageIdentifier (mkPackageName $ T.unpack pname) pvers

        me <- gets $ MM.lookup pid
        case revtype of
          MetadataRev -> do
            -- liftIO $ putStrLn $ Pretty.prettyShow pid
            (either (error . show) (const $ pure ()) =<<) $ liftIO $ flip run conn $ statement () $ Rel8.run $ runSqlM $ do
              -- _ <- mkUser (UserId $ fromIntegral $ Tar.ownerId o) (T.pack $ Tar.ownerName o)
              newMetaRev pid (maybe 0 metaRev me) e
          TarballRev -> pure ()

        modify' $ mappend $ MM.singleton pid $
          case revtype of
            TarballRev -> mempty { rs_tar_rev = 1 }
            MetadataRev -> mempty { rs_meta_rev = 1 }
      m) (pure ()) (liftIO . print) es


backfillPackageDB :: FilePath -> IO ()
backfillPackageDB dbDir = do
  packagesH <- openLocalStateFrom (dbDir </> "PackagesState") (initialPackagesState False)

  PackagesState (PackageIndex pkgs) _ <- query packagesH GetPackagesState

  closeAcidState packagesH

  mkConn $ \conn -> do
    flip evalStateT (mempty @(MonoidalMap PackageIdentifier RevState)) $
      (either (error . show) (const $ pure ()) =<<) $ liftIO $
        flip run conn $ statement () $
          Rel8.run $ runSqlM $ do
            for_ pkgs $ traverse insertPkgInfo
            pure $ pure $ lit True


backfillTarIndex :: FilePath -> IO ()
backfillTarIndex blobPath = do
  mkConn $ \conn -> do
    Right nogzs <-
      flip run conn $ statement () $ Rel8.run $ select $ do
        r <- each packageTarballRevisionsSchema
        pure $ tarballBlobNoGz r
    store <- Blob.open blobPath
    for_ nogzs $ \blob -> do
      Right bid <- pure $ Blob.readBlobId $ show $ getBlobId blob
      bs <- BSL.readFile $ Blob.filepath store bid
      let es = Tar.read bs
      insertTarEntries blob es conn

