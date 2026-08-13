{-# LANGUAGE PackageImports #-}

module Hackage.SyncAPI.Sync where

import Codec.Archive.Tar qualified as Tar
import Control.Monad.Except
import Control.Monad.Reader
import Data.BlobStorage qualified as Blob
import Data.ByteString.Lazy qualified as BSL
import Data.Map qualified as M
import Data.TarIndex
import Data.Text qualified as T
import Hackage.Schemas.Packages
import Hackage.ServerM
import Hackage.SyncAPI.Type
import Hackage.Types
import Hackage.Types.PrimaryKey
import Hackage.Utils
import Rel8 hiding (run)
import Servant.Server
import Servant.Server.Generic (AsServerT)
import Servant.Tarball


syncServer :: SyncApi (AsServerT ServerM)
syncServer = SyncApi
  { sync_api_index_blob = syncBlob
  }


syncBlob :: BlobId Tarball -> ServerM ()
syncBlob bid = do
  store <- asks serverBlobStore
  bs <- liftIO $ BSL.readFile $ Blob.filepath store bid
  let es = Tar.read bs
  insertTarEntries bid es


insertTarEntries
  :: BlobId Tarball
  -> Entries e
  -> ServerM ()
insertTarEntries bid es = do
  case construct es of
    Left _err -> throwError err500
    Right m ->
      liftDB $ doInsert_ $
        Insert
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
          , returning = NoReturning
          }


