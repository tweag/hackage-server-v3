{-# LANGUAGE OverloadedStrings #-}

module Hackage.SyncAPI.Type where

import Data.Aeson
import GHC.Generics
import Servant.API
import Hackage.Types


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

instance FromJSON NewUserReq where
  parseJSON = withObject "NewUserReq" $ \obj ->
    NewUserReq
      <$> obj .: "username"
      <*> obj .: "id"

