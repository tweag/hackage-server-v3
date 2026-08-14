{-# OPTIONS_GHC -Wno-x-dontuse #-}

module Hackage.TarIndex where

import Data.Bifunctor (bimap)
import Codec.Archive.Tar qualified as Tar
import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.BlobStorage qualified as Blob
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable
import Data.Map qualified as M
import Data.TarIndex
import Data.Text qualified as T
import Hackage.Schemas.Packages
import Hackage.Types
import Hackage.Types.PrimaryKey
import Hackage.Utils
import Rel8 hiding (indexed)
import Servant.Tarball


indexingTarIndices
    :: Serializable exprs (FromExprs exprs)
    => Blob.BlobStorage
    -> Query (Expr (BlobId Tarball))
    -> (Expr (BlobId Tarball) -> TarIndexRow Expr -> Query exprs)
    -> DatabaseM [FromExprs exprs]
indexingTarIndices store blobsQ k = do
  blobs <- doSelect $ do
    blob <- blobsQ
    indexed <- exists $ do
      idxd <- each tarAlreadyIndexedSchema
      where_ $ tarAlreadyIndexedBlob idxd  ==. blob
      pure ()
    pure (blob, indexed)

  for_ blobs $ \(bid, indexed) -> unless indexed $ do
    bs <- liftIO $ BSL.readFile $ Blob.filepath store bid

    key <- doInsert1 $
      Insert
        { into = tarAlreadyIndexedSchema
        , rows = pure $ TarAlreadyIndexedRow
            { tarAlreadyIndexedId = newPrimaryKey
            , tarAlreadyIndexedBlob = lit bid
            }
        , onConflict = Abort
        , returning = Returning tarAlreadyIndexedId
        }

    doInsert_ $
      Insert
        { into = tarIndexSchema
        , rows = do
            (path, off) <- values $ do
              (path, v) <-
                M.toList $ case construct $ Tar.read bs of
                  Left _err ->
                    -- TODO(sandy): Log the error
                    mempty
                  Right es -> es
              pure $ lit (T.pack path, v)
            pure $ TarIndexRow
              { tarIndexId = newPrimaryKey
              , tarIndexKey = lit key
              , tarIndexPath = path
              , tarIndexOffset = off
              }
        , onConflict = DoNothing
        , returning = NoReturning
        }

  doSelect $ do
    blob <- values $ fmap (lit . fst) blobs
    idxd <- each tarAlreadyIndexedSchema
    where_ $ tarAlreadyIndexedBlob idxd ==. blob
    idx <- each tarIndexSchema
    where_ $ tarIndexKey idx ==. tarAlreadyIndexedId idxd
    k blob idx

