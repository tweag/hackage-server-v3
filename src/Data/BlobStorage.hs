module Data.BlobStorage where

import Data.Foldable
import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import System.FilePath ((</>))
import Data.Serialize (Serialize, encode, decode)
import Distribution.Utils.MD5 (md5, showMD5)
import System.Directory
import Hackage.Types
import GHC.Fingerprint


newtype BlobStorage = BlobStorage
  { getBlobStoragePath :: FilePath
  }

incomingDir :: BlobStorage -> FilePath
incomingDir (BlobStorage storeDir) = storeDir </> "incoming"

-- | Opens an existing or new blob storage area.
--
open :: FilePath -> IO BlobStorage
open storeDir = do
    let store   = BlobStorage storeDir
        chars   = ['0' .. '9'] ++ ['a' .. 'f']
        subdirs = incomingDir store
                : [storeDir </> [x, y] | x <- chars, y <- chars]

    exists <- doesDirectoryExist storeDir
    if not exists
      then do
        createDirectory storeDir
        for_ subdirs createDirectory
      else
        for_ subdirs $ \d -> do
          subdirExists <- doesDirectoryExist d
          unless subdirExists $
            fail $ unwords
              [ "Store directory"
              , show storeDir
              , "exists but"
              , show d
              , "does not"
              ]
    return store

-- | Which directory do we store a blob ID in?
directory :: BlobStorage -> BlobId a -> FilePath
directory (BlobStorage storeDir) (BlobId hash) =
  storeDir </> take 2 (showMD5 hash)

filepath :: BlobStorage -> BlobId a -> FilePath
filepath store bid@(BlobId hash) =
  directory store bid </> showMD5 hash


copyTo :: BlobStorage -> FilePath -> IO (BlobId a)
copyTo store fp = do
  hash <- getFileHash fp
  let bid = BlobId hash
  copyFile fp $ filepath store bid
  pure bid


add :: Serialize a => BlobStorage -> a -> IO (BlobId a)
add store a = do
  let encoded = encode a
      bid = BlobId $ md5 encoded
  BS.writeFile (filepath store bid) encoded
  pure bid

get' :: BlobStorage -> BlobId a -> IO BS.StrictByteString
get' store = BS.readFile . filepath store

get :: BlobStorage -> BlobId a -> IO BSL.LazyByteString
get store = BSL.readFile . filepath store

fetch :: Serialize a => BlobStorage -> BlobId a -> IO (Either String a)
fetch store = fmap decode . BS.readFile . filepath store

unsafeFetch :: Serialize a => BlobStorage -> BlobId a -> IO a
unsafeFetch store bid = either error pure =<< fetch store bid

