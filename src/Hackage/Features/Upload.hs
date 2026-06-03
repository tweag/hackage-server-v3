{-# LANGUAGE OverloadedStrings #-}

module Hackage.Features.Upload where

import qualified Text.Blaze.Html4.Strict as H
import Hackage.Schemas.Users
import Data.Functor
import qualified Data.Map as M
import Data.Map (Map)
import Data.Aeson
import Rel8
import Hackage.Utils
import Hackage.Types
import Servant.Server
import Servant.API
import Servant.HTML.Blaze

data TrusteesObject = TrusteesObject (Map UserId UserName)

instance ToJSON TrusteesObject where
  toJSON (TrusteesObject ts) = object
    [ "members" .= (M.toList ts <&> \(uid, name) ->
        object
          [ "userid" .= uid
          , "username" .= name
          ]
      )
    , "title" .= id @String "Package trustees"
    , "description" .= id @String
        "The role of trustees is to help to curate the whole package collection. Trustees have a limited ability to edit package information, for the entire package database (as opposed to package maintainers who have full control over individual packages). Trustees can edit .cabal files, edit other package metadata and upload documentation but they cannot upload new package versions."
    ]


trusteesEndpoint :: Connection -> Handler DualContent
trusteesEndpoint conn = do
  ts <- doSelectE conn $ do
    r <- each userRolesSchema
    where_ $ userRoleRole r ==. lit Trustee
    u <- activeUsers
    where_ $ userId u ==. userRoleUserId r
    pure (userRoleUserId r, userName u)

  pure $ DualContent
    { json = toJSON $ TrusteesObject $ M.fromList ts
    , html = H.p "hello world"
    }

