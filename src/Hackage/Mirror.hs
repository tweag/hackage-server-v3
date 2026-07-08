{-# LANGUAGE OverloadedStrings #-}

module Hackage.Mirror where

import GHC.Generics
import qualified Rel8 as Rel8
import Hasql.Session (statement, run)
import TestAPI (mkConn)
import Import
import Control.Monad (when, void)
import Distribution.Pretty qualified as Pretty
import Data.Coerce
import GHC.Generics
import Distribution.Types.PackageId
import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Types qualified as Tar
import Codec.Compression.Zlib qualified as Zlib
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map.Monoidal qualified as MM
import Data.Map.Monoidal (MonoidalMap)
import Control.Monad.State
import Hackage.Types
import Data.Monoid(Sum(..))
import Data.Int
import Data.Text qualified as T
import Distribution.Types.Version (mkVersion)
import Distribution.Types.PackageName
import Rel8 hiding (run)
import Data.Time.Clock.POSIX
import Hackage.Schemas.Packages (PkgRevId)



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

newMetaRev :: PackageIdentifier -> MetadataRevIx -> Tar.GenEntry BSL.ByteString b c -> SqlM (Query (Expr PkgRevId))
newMetaRev pid rev e = do
  pkgid <- mkPkgIdentifier pid
  mkMetadataRev
    pkgid
    rev
    (case Tar.entryContent e of
       Tar.NormalFile x y -> BS.toStrict x
       _ -> error "something else"
    )
    ( posixSecondsToUTCTime $ fromIntegral $ Tar.entryTime e
    , UserId $ fromIntegral $ Tar.ownerId $ Tar.entryOwnership e
    )


data UpdateType = TarballRev | MetadataRev
  deriving stock (Eq, Ord, Show)

main :: IO ()
main = mkConn $ \conn -> do
  bs <- BSL.readFile "/home/sandy/01-index.tar"
  let es = Tar.read bs
  flip evalStateT (mempty @(MonoidalMap PackageIdentifier RevState)) $
    Tar.foldEntries (\e m -> do
      let o = Tar.entryOwnership e
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
            (either (error . show) (const $ pure ()) =<<) $ liftIO $ flip run conn $ statement () $ Rel8.run $ runSqlM $ newMetaRev pid (maybe 0 metaRev me) e
          TarballRev -> pure ()

        modify' $ mappend $ MM.singleton pid $
          case revtype of
            TarballRev -> mempty { rs_tar_rev = 1 }
            MetadataRev -> mempty { rs_meta_rev = 1 }
        -- liftIO $ putStrLn $ unwords
        --   [ Tar.entryPath e
        --   , Tar.ownerName o
        --   , show $ Tar.ownerId o
        --   ]
      m
                    ) (pure ()) (liftIO . print) es

