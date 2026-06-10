module Data.TarIndex
  ( construct
  , TarEntryOffset
  , Entries
  ) where

import Data.Int (Int64)
import Codec.Archive.Tar (Entry, GenEntry(..), GenEntryContent(..), Entries, GenEntries(..), entryPath)
import Data.Map (Map)
import Data.Map qualified as M


type TarEntryOffset = Int64


unfoldEntries :: Entries e -> Either e [Entry]
unfoldEntries Done = pure []
unfoldEntries (Fail err) = Left err
unfoldEntries (Next e es) = fmap (e :) $ unfoldEntries es


construct :: Entries e -> Either e (Map FilePath TarEntryOffset)
construct entries = do
  es <- unfoldEntries entries
  let offsets = scanr (\e offset -> offset + sizeOf e + 1) 0 es
  pure $ mconcat $ do
    (e, offset) <- zip es offsets
    pure $ M.singleton (entryPath e) offset


sizeOf :: Entry -> TarEntryOffset
sizeOf entry = do
  let blocks size = 1 + ((fromIntegral size - 1) `div` 512)
  case entryContent entry of
    NormalFile     _   size -> blocks size
    OtherEntryType _ _ size -> blocks size
    _                       -> 0

