{-# LANGUAGE OverloadedStrings #-}

module Hackage.SyncAPI.Type where

import Data.Aeson
import Data.ByteString (StrictByteString)
import Data.Coerce (coerce)
import Data.Int (Int64)
import Data.Profunctor (dimap)
import Data.Schema qualified as S
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Time
import GHC.Generics
import Hackage.Objects (Schema(..))
import Hackage.Types
import Servant.API
import Servant.Tarball


-- | An API for synchrozing realtime data events from hackage-server v2.
data SyncApi mode = SyncApi
  { sync_api_new_user :: mode
      :- "api"
      :> "sync"
      :> "v1"
      :> "users"
      :> ReqBody '[JSON] NewUserReq
      :> Post '[JSON] ()
  , sync_api_new_package :: mode
      :- "api"
      :> "sync"
      :> "v1"
      :> "packages"
      :> Capture "package" PackageId
      :> ReqBody '[JSON] NewPackageReq
      :> Post '[JSON] ()
  , sync_api_revise_meta :: mode
      :- "api"
      :> "sync"
      :> "v1"
      :> "packages"
      :> Capture "package" PackageId
      :> "revision"
      :> Capture "revision" MetadataRevIx
      :> ReqBody '[JSON] ReviseMetaReq
      :> Post '[JSON] ()
  , sync_api_revise_tarball :: mode
      :- "api"
      :> "sync"
      :> "v1"
      :> "packages"
      :> Capture "package" PackageId
      :> "tarball"
      :> Capture "revision" TarballRevIx
      :> ReqBody '[JSON] ReviseTarballReq
      :> Post '[JSON] ()
  }
  deriving Generic


--------------------------------------------------------------------------------
-- POST /users

data NewUserReq = NewUserReq
  { nur_username :: UserName
  , nur_userid :: UserId
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (ToJSON, FromJSON) via Schema NewUserReq


instance S.ToSchema NewUserReq where
  schema = S.object $
    NewUserReq
      <$> nur_username S..=
            S.field "username"
              (dimap coerce coerce $ S.text "UserName")
      <*> nur_userid S..=
            S.field "id"
              (dimap coerce coerce $ S.schema @Int64)


--------------------------------------------------------------------------------
-- POST /packages/:package/revision/:revision

data NewPackageReq = NewPackageReq
  { npr_uploader :: UserId
  , npr_uploadTime :: UTCTime
  , npr_cabalFile :: StrictByteString
  , npr_blobGz :: BlobId (Compressed Tarball)
  , npr_blobNoGz :: BlobId Tarball
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (ToJSON, FromJSON) via Schema NewPackageReq

instance S.ToSchema NewPackageReq where
  schema = S.object $
    NewPackageReq
      <$> npr_uploader S..=
            S.field "uploader"
              (dimap coerce coerce $ S.schema @Int64)
      <*> npr_uploadTime S..=
            S.field "uploadTime" S.json
      <*> npr_cabalFile S..=
            S.field "cabalFile" (dimap decodeUtf8 encodeUtf8 $ S.text "UserName")
      <*> npr_blobGz S..=
            S.field "blobGz" S.schema
      <*> npr_blobNoGz S..=
            S.field "blobNoGz" S.schema


--------------------------------------------------------------------------------
-- POST /packages/:package/revision/:revision

data ReviseMetaReq = ReviseMetaReq
  { rmr_uploader :: UserId
  , rmr_uploadTime :: UTCTime
  , rmr_cabalFile :: StrictByteString
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (ToJSON, FromJSON) via Schema ReviseMetaReq

instance S.ToSchema ReviseMetaReq where
  schema = S.object $
    ReviseMetaReq
      <$> rmr_uploader S..=
            S.field "uploader"
              (dimap coerce coerce $ S.schema @Int64)
      <*> rmr_uploadTime S..=
            S.field "uploadTime" S.json
      <*> rmr_cabalFile S..=
            S.field "cabalFile" (dimap decodeUtf8 encodeUtf8 $ S.text "UserName")


--------------------------------------------------------------------------------
-- POST /packages/:package/revision/:revision

data ReviseTarballReq = ReviseTarballReq
  { rtr_uploader :: UserId
  , rtr_uploadTime :: UTCTime
  , rtr_blobGz :: BlobId (Compressed Tarball)
  , rtr_blobNoGz :: BlobId Tarball
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (ToJSON, FromJSON) via Schema ReviseTarballReq

instance S.ToSchema ReviseTarballReq where
  schema = S.object $
    ReviseTarballReq
      <$> rtr_uploader S..=
            S.field "uploader"
              (dimap coerce coerce $ S.schema @Int64)
      <*> rtr_uploadTime S..=
            S.field "uploadTime" S.json
      <*> rtr_blobGz S..=
            S.field "blobGz" S.schema
      <*> rtr_blobNoGz S..=
            S.field "blobNoGz" S.schema

