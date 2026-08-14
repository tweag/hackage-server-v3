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
  pure ()

