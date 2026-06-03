{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hackage.Features.Upload where

import Data.Aeson hiding (Result(..))
import Data.Functor
import Data.Map (Map)
import Hackage.Schemas.Users
import Hackage.Types
import Hackage.Utils
import Rel8 hiding (Lift)
import Servant.EDE
import Servant.Server
import qualified Data.Map as M

data TrusteesObject = TrusteesObject (Map UserId UserName)

instance ToJSON TrusteesObject where
  toJSON ts = Object $ toObject ts <>
    [ "title" .= id @String "Package trustees"
    , "description" .= id @String "The role of trustees is to help to curate the whole package collection. Trustees have a limited ability to edit package information, for the entire package database (as opposed to package maintainers who have full control over individual packages). Trustees can edit .cabal files, edit other package metadata and upload documentation but they cannot upload new package versions."
    ]

instance ToObject TrusteesObject where
  toObject (TrusteesObject ts) =
    [ "members" .= (M.toList ts <&> \(uid, name) ->
        object
          [ "userid" .= uid
          , "username" .= name
          ]
      )
    ]


trusteesEndpoint :: Connection -> Handler TrusteesObject
trusteesEndpoint conn = do
  ts <- doSelectE conn $ do
    r <- each userRolesSchema
    where_ $ userRoleRole r ==. lit Trustee
    u <- activeUsers
    where_ $ userId u ==. userRoleUserId r
    pure (userRoleUserId r, userName u)

  pure $ TrusteesObject $ M.fromList ts

