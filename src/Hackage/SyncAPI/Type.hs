module Hackage.SyncAPI.Type where

import GHC.Generics
import Servant.API
import Hackage.Types
import Servant.Tarball


-- | An API for synchrozing realtime data events from hackage-server v2.
data SyncApi mode = SyncApi
  { sync_api_index_blob :: mode :- "blobs" :> Capture "blob" (BlobId Tarball) :> Post '[JSON] ()
    -- ^ Index a new blob in the blob store.
  }
  deriving Generic

