{-# LANGUAGE OverloadedStrings #-}

module Servant.Tarball where

import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Media ((//))
import Servant

data Tarball

instance Accept Tarball where
  contentType _ = "application" // "gzip"

instance MimeRender Tarball BL.ByteString where
  mimeRender _ = id
