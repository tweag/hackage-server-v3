{-# LANGUAGE OverloadedStrings #-}

module Hackage.SyncAPI.Type where

import Data.Aeson
import Data.Coerce (coerce)
import Data.Int (Int64)
import Data.Profunctor (dimap)
import Data.Schema qualified as S
import GHC.Generics
import Hackage.Objects (Schema(..))
import Hackage.Types
import Servant.API


-- | An API for synchrozing realtime data events from hackage-server v2.
data SyncApi mode = SyncApi
  { sync_api_new_user :: mode
      :- "api"
      :> "sync"
      :> "v1"
      :> "users"
      :> ReqBody '[JSON] NewUserReq
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

