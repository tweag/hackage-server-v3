{-# LANGUAGE AllowAmbiguousTypes             #-}
{-# LANGUAGE OverloadedLabels                #-}
{-# LANGUAGE OverloadedStrings               #-}
{-# LANGUAGE PartialTypeSignatures           #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Import where

import Hackage.Import
import Data.Coerce
import Data.Foldable
import Data.Generics.Labels ()
import Data.Int (Int64)
import Hackage.Types

import Distribution.Server.Framework.BlobStorage qualified as V2
import Distribution.Server.Users.Types qualified as V2
import Distribution.Server.Packages.Types qualified as V2


insertPkgInfo :: V2.PkgInfo -> SqlM ()
insertPkgInfo (V2.PkgInfo pkgid mdrevs tbrevs) = do
  epkgid <- mkPkgIdentifier pkgid
  for_ (zip (toList mdrevs) $ coerce [id @Int64 0..]) $
    \((V2.CabalFileText cabal, (a, V2.UserId b)), revix) ->
      mkMetadataRev epkgid revix cabal a $ UserId $ fromIntegral b

  let convertBlobId = either error BlobId . parseMD5 . V2.blobMd5
  for_ (zip (toList tbrevs) [0..]) $
    \(((V2.PkgTarball (V2.BlobInfo gz _ _) nogz), (time, V2.UserId uid)), revix) ->
      mkTarballRev
        epkgid
        revix
        (convertBlobId gz)
        (convertBlobId nogz)
        time $ UserId $ fromIntegral uid

