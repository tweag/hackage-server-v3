{-# LANGUAGE AllowAmbiguousTypes             #-}
{-# LANGUAGE OverloadedLists                 #-}
{-# LANGUAGE OverloadedStrings               #-}
{-# LANGUAGE PartialTypeSignatures           #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Import where

import Data.Foldable
import Data.Text.Encoding (decodeUtf8)
import Data.Function (on)
import Distribution.Types.Version
import Hackage.Types.PrimaryKey
import Distribution.Server.Packages.Types hiding (pkgInfoId)
import Distribution.Server.Features.UserDetails.Types qualified as V2
import Distribution.Server.Users.Types qualified as V2
import Distribution.Server.Packages.Types qualified as V2
import Distribution.Server.Framework.BlobStorage qualified as V2
-- import Data.TarIndex
import Distribution.Package (PackageIdentifier(..), unPackageName)
import Distribution.Package qualified as Cabal
import Rel8
import Hackage.Types
import Hackage.Schemas.Packages
import Hackage.Schemas.Users
import Data.Text qualified as T
import Data.Time


noUpsert
    :: (Projecting names index, _)
    => Projection names index
    -> (Transpose Expr names -> Expr a)
    -> OnConflict names
noUpsert idx f = DoUpdate $ Upsert
  { index = idx
  , predicate = Nothing
  , set = const id
  , updateWhere = on (==.) f
  }



mkPkgName :: PackageName -> Statement (Query (Expr PkgId))
mkPkgName name = insert $
  Insert
    { into = packageNameSchema
    , rows = values @_ @[]
        [ PackageNameRow
            { packageNameId = newPrimaryKey
            , packageName = lit $ T.pack $ unPackageName name
            }
        ]
    , onConflict = noUpsert packageName packageNameId
    , returning = Returning packageNameId
    }

mkPkgIdentifier :: PackageIdentifier -> Statement (Query (Expr PkgInfoId))
mkPkgIdentifier pkgid = do
  pkgname <- mkPkgName $ Cabal.packageName pkgid
  insert $
    Insert
      { into = pkgInfoSchema
      , rows = do
          pkgnameid <- pkgname
          values @_ @[]
            [ PkgInfoRow
                { pkgInfoId = newPrimaryKey
                , pkgId = pkgnameid
                , packageVersion = lit $ Cabal.packageVersion pkgid
                , pkgInfoDeprecated = lit False
                }
            ]
      , onConflict = noUpsert (liftA2 (,) pkgId packageVersion) pkgInfoId
      , returning = Returning pkgInfoId
      }


mkMetadataRev
    :: Query (Expr PkgInfoId)
    -> V2.MetadataRevIx
    -> CabalFileText
    -> OldUploadInfo
    -> Statement (Query (Expr PkgRevId))
mkMetadataRev qpkgid (V2.MetadataRevIx revix) (CabalFileText cabal) (time, V2.UserId uid) = do
  insert $ Insert
    { into = metadataRevisionsSchema
    , rows = do
        pkgid <- qpkgid
        values @_ @[]
          [ MetadataRevisionRow
              { metadataId = newPrimaryKey
              , metadataPkgId = pkgid
              , metadataRevId = lit $ fromIntegral revix
              , metadataTime = lit time
              , metadataUploader = lit $ UserId $ fromIntegral uid
              , metadataCabalFile = lit $ decodeUtf8 cabal
              }
          ]
    , onConflict = DoNothing
    , returning = Returning metadataId
    }


mkTarballRev
  :: Query (Expr PkgInfoId)
  -> V2.TarballRevIx
  -> PkgTarball
  -> OldUploadInfo
  -> Statement (Query (Expr TarballRevId))
mkTarballRev qpkgid (V2.TarballRevIx revix) (PkgTarball (BlobInfo gz len sha) nogz) (time, V2.UserId uid) =
  insert $ Insert
    { into = packageTarballRevisionsSchema
    , rows = do
        pkgid <- qpkgid
        values @_ @[]
          [ TarballRevisionRow
              { tarballRevId = newPrimaryKey
              , tarballPkgId = pkgid
              , tarballRevIx = lit $ fromIntegral revix
              , tarballTime = lit time
              , tarballUploader = lit $ UserId $ fromIntegral uid
              , tarballBlobGz
                  = lit $ either error BlobId $ parseMD5 $ V2.blobMd5 gz
              , tarballBlobNoGz
                  = lit $ either error BlobId $ parseMD5 $ V2.blobMd5 nogz
              , tarballGzLength = lit $ fromIntegral len
              , -- TODO(sandy): fixme
                tarballGzHash = lit $ T.pack $ show sha
              }
          ]
    , onConflict = DoNothing
    , returning = Returning tarballRevId
    }
mkTarballRev _ (V2.TarballRevIx _) e _ = error $ show e

mkUser
  :: V2.UserId
  -> V2.UserName
  -> V2.AccountDetails
  -> Statement (Query (Expr UserId))
mkUser (V2.UserId uid) (V2.UserName uname) details = insert $ Insert
  { into = usersSchema
  , rows = values @_ @[]
      [ lit $ UsersRow
          { userId = UserId $ fromIntegral uid
          , userName = T.pack uname
          , userEmail = Just $ V2.accountContactEmail details
          , userRealName = Just $ V2.accountName details
          }
      ]
  , onConflict = noUpsert userName userId
  , returning = Returning userId
  }

sometime :: UTCTime
sometime = read "2026-06-23 21:45:05.410012761 UTC"

main :: IO ()
main = putStrLn $ showStatement $ insertPkgInfo $
  PkgInfo
    (PackageIdentifier "dope" $ mkVersion [1,2,3])
    [ (CabalFileText "v1", (sometime, V2.UserId 111))
    , (CabalFileText "v2", (sometime, V2.UserId 122))
    , (CabalFileText "v3", (sometime, V2.UserId 333))
    ]
    []

insertPkgInfo :: PkgInfo -> Statement ()
insertPkgInfo (PkgInfo pkgid mdrevs tbrevs) = do
  epkgid <- mkPkgIdentifier pkgid
  for_ (zip (toList mdrevs) [0..]) $ \((cabal, oui), revix) ->
    mkMetadataRev epkgid (V2.MetadataRevIx revix) cabal oui
  for_ (zip (toList tbrevs) [0..]) $ \((pkgtb, oui), revix) ->
    mkTarballRev epkgid (V2.TarballRevIx revix) pkgtb oui

