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
import Data.IntMap qualified as IM
import Data.Map qualified as M
import Data.Map.Monoidal (MonoidalMap)
import Data.Monoid(Sum(..))
import Data.Text qualified as T
import Data.Time.Clock.POSIX
import Distribution.Server.Features.Core.State (initialPackagesState, GetPackagesState(..), PackagesState(..))
import Distribution.Server.Features.PreferredVersions.State (PreferredVersions(..), PreferredInfo(..), GetPreferredVersions(..), initialPreferredVersions)
import Distribution.Server.Packages.PackageIndex (PackageIndex(..))
import Distribution.Server.Users.State (GetUserDb(..))
import Distribution.Server.Users.Types qualified as V2
import Distribution.Server.Users.Users qualified as Users
import Distribution.Types.PackageId
import GHC.Generics (Generic, Generically(..))
import Hackage.Import
import Hackage.Schemas.Packages (PkgRevId, pkgInfoSchema, PkgInfoRow(..), packageNameSchema, PackageNameRow(..))
import Hackage.Types
import Hackage.Utils (Connection)
import Hasql.Session (statement, run)
import Import
import Rel8 hiding (run)
import Rel8 qualified as Rel8
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
    DoNothing
    pkgid
    rev
    (case Tar.entryContent e of
       Tar.NormalFile x _ -> BS.toStrict x
       _ -> error "Found something in the Tar that isn't a file"
    )
    (posixSecondsToUTCTime $ fromIntegral $ Tar.entryTime e)
    (UserId $ fromIntegral $ Tar.ownerId $ Tar.entryOwnership e)


backfillPackageDB :: Connection -> FilePath -> IO ()
backfillPackageDB conn dbDir = do
  usersH    <- openLocalStateFrom (dbDir </> "Users") Users.emptyUsers
  packagesH <- openLocalStateFrom (dbDir </> "PackagesState") (initialPackagesState False)
  preferredH <- openLocalStateFrom (dbDir </> "PreferredVersions") (initialPreferredVersions True)

  Users.Users users _ _ _ <- query usersH GetUserDb
  PackagesState (PackageIndex pkgs) _ <- query packagesH GetPackagesState
  PreferredVersions preferred _deprecated _ <- query preferredH GetPreferredVersions

  closeAcidState packagesH
  closeAcidState usersH
  closeAcidState preferredH

  flip evalStateT (mempty @(MonoidalMap PackageIdentifier RevState)) $
    (either (error . show) (const $ pure ()) =<<) $ liftIO $
      flip run conn $ statement () $
        Rel8.run $ runSqlM $ do
          -- Import all of the users
          for_ (IM.toList users) $ \(uid, (V2.UserInfo (V2.UserName uname) _ _)) ->
            mkUser (UserId $ fromIntegral uid) $ UserName $ T.pack uname

          -- Import all pkginfos
          for_ pkgs $ traverse insertPkgInfo

          -- Update the pkginfos to set the deprecated flag if necessary
          for_ (M.toList preferred) $ \(pkgname, pref) ->
            for_ (deprecatedVersions pref) $ \v ->
              sql $ update $ Update
                { target = pkgInfoSchema
                , from = do
                    p <- each packageNameSchema
                    where_ $ packageName p ==. lit pkgname
                    pure $ packageNameId p
                , set = \_ pkginfo ->
                    pkginfo { pkgInfoDeprecated = lit True }
                , updateWhere = \pkg pkginfo ->
                    pkgId pkginfo ==. pkg &&.
                      packageVersion pkginfo ==. lit v
                , returning = NoReturning
                }
          pure $ pure $ lit True

