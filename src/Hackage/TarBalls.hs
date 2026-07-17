{-# LANGUAGE OverloadedStrings #-}

module Hackage.TarBalls where

import Distribution.Utils.MD5 (md5)
import Codec.Archive.Tar qualified as Tar
import Data.ByteString.Lazy qualified as BSL
import Codec.Compression.Zlib
import Control.Monad.Except
import Data.Map qualified as M
import Data.TarIndex
import Data.Text qualified as T
import Hackage.Schemas.Packages
import Hackage.Types
import Hackage.Types.PrimaryKey
import Hackage.Utils
import Rel8 (Insert(..), Returning(..), OnConflict(..), lit, values)
import Servant.Tarball
import TestAPI (mkConn)


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

main :: IO ()
main = mkConn $ \conn -> do
  x <- BSL.readFile "/home/sandy/rel8-1.0.0.0.tar"
  let z = Tar.read x
  print =<< insertTarEntries (BlobId $ md5 "rel8-1.0.0.0.tar.gz") z conn

