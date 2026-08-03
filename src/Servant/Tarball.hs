{-# LANGUAGE OverloadedStrings #-}

module Servant.Tarball where

import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Media ((//))
import Servant

data Tarball

instance Accept Tarball where
  contentType _ = "application" // "x-tar"

instance MimeRender Tarball BL.ByteString where
  mimeRender _ = id


data Compressed a

instance Accept (Compressed a) where
  contentType _ = "application" // "gzip"

instance MimeRender (Compressed a) BL.ByteString where
  mimeRender _ = id
