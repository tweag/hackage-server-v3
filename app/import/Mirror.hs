{-# LANGUAGE OverloadedStrings #-}

module Mirror where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Control.Monad.State
import Data.Acid (openLocalStateFrom, query, closeAcidState)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Coerce
import Data.Foldable
import Data.Int
import Data.Map.Monoidal (MonoidalMap)
import Data.Monoid(Sum(..))
import Data.Time.Clock.POSIX
import Distribution.Server.Features.Core.State (initialPackagesState, GetPackagesState(..), PackagesState(..))
import Distribution.Server.Framework.BlobStorage qualified as Blob
import Distribution.Server.Packages.PackageIndex (PackageIndex(..))
import Distribution.Types.PackageId
import GHC.Generics
import Hackage.Schemas.Packages (PkgRevId, TarballRevisionRow(..), packageTarballRevisionsSchema)
import Hackage.Types
import Hackage.Utils (Connection)
import Hasql.Session (statement, run)
import Import
import Rel8 hiding (run)
import Rel8 qualified as Rel8
import System.FilePath
import Tarballs (insertTarEntries)


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


backfillPackageDB :: Connection -> FilePath -> IO ()
backfillPackageDB conn dbDir = do
  packagesH <- openLocalStateFrom (dbDir </> "PackagesState") (initialPackagesState False)

  PackagesState (PackageIndex pkgs) _ <- query packagesH GetPackagesState

  closeAcidState packagesH

  flip evalStateT (mempty @(MonoidalMap PackageIdentifier RevState)) $
    (either (error . show) (const $ pure ()) =<<) $ liftIO $
      flip run conn $ statement () $
        Rel8.run $ runSqlM $ do
          for_ pkgs $ traverse insertPkgInfo
          pure $ pure $ lit True


backfillTarIndex :: Connection -> FilePath -> IO ()
backfillTarIndex conn blobPath = do
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

