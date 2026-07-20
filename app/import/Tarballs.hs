{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module Tarballs where

import Control.Monad.Except
import Data.Map qualified as M
import "hackage-server-v3" Data.TarIndex
import Data.Text qualified as T
import Hackage.Schemas.Packages
import Hackage.Types
import Hackage.Types.PrimaryKey
import Hackage.Utils
import Rel8 (Insert(..), Returning(..), OnConflict(..), lit, values)
import Servant.Tarball


data InsertTarEntriesError e
  = TarDecodingError e
  | DatabaseError SessionError
  deriving stock Show


insertTarEntries
  :: BlobId Tarball
  -> Entries e
  -> Connection
  -> IO (Either (InsertTarEntriesError e) [TarIndexId])
insertTarEntries bid es conn = runExceptT $ do
  m <- withExceptT TarDecodingError $ liftEither $ construct es
  withExceptT DatabaseError $ ExceptT $ flip doInsert conn $
    Insert
      { into = tarIndexSchema
      , rows = do
          (path, offset) <- values $ do
            (k, v) <- M.toList m
            pure $ lit (T.pack k, v)
          pure $ TarIndexRow
            { tarIndexId = newPrimaryKey
            , tarIndexBlob = lit bid
            , tarIndexPath = path
            , tarIndexOffset = offset
            }
      , onConflict = DoNothing
      , returning = Returning tarIndexId
      }

