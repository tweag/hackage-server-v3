{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module Tarballs where

import Control.Monad.Except
import Control.Monad.Reader (runReaderT)
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
  withExceptT DatabaseError $ flip runReaderT conn $ unDatabaseM $ do
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

    doInsert $
      Insert
        { into = tarIndexSchema
        , rows = do
            (path, off) <- values $ do
              (k, v) <- M.toList m
              pure $ lit (T.pack k, v)
            pure $ TarIndexRow
              { tarIndexId = newPrimaryKey
              , tarIndexKey = lit key
              , tarIndexPath = path
              , tarIndexOffset = off
              }
        , onConflict = DoNothing
        , returning = Returning tarIndexId
        }


