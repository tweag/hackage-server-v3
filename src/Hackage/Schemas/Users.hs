{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Hackage.Schemas.Users
  ( -- * Users table
    UsersRow(..)
  , type User
  , usersSchema
  , UserStatus (..)
  , activeUsers
  , usersTable

    -- * User roles junction table
  , UserRoleRow(..)
  , userRolesSchema
  , userRolesTable

    -- * User auth tokens table
  , UserAuthTokenRow(..)
  , userAuthTokensSchema
  , userAuthTokensTable

    -- * Role type
  , UserRole(..)
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Hackage.Auth.AuthToken
import Hackage.Types
import Hackage.Types.PrimaryKey

import Rel8
  ( Rel8able
  , Result
  , TableSchema(..)
  , Column
  , Name
  , ReadShow(..)
  , DBType
  , DBEq
  , Query
  , Expr
  , each
  , (==.)
  , where_
  , lit
  )
import Rel8.CreateTable

data UsersRow f = UsersRow
  { userId :: Column f UserId
  , userName :: Column f UserName
  , userEmail :: Column f (Maybe Text)
  , userRealName :: Column f (Maybe Text)
  , userAuth :: Column f PasswdHash
  , userStatus :: Column f UserStatus
  , userAdminNotes :: Column f Text
  , userCreatedTime :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance Show (UsersRow Result)

type User = UsersRow Result

usersSchema :: TableSchema (UsersRow Name)
usersSchema = TableSchema
  { name = "users"
  , columns = UsersRow
      { userId = "user_id"
      , userName = "user_name"
      , userEmail = "user_email"
      , userRealName = "user_real_name"
      , userAuth = "user_auth"
      , userStatus = "user_status"
      , userAdminNotes = "user_admin_notes"
      , userCreatedTime = "user_created_time"
      }
  }

activeUsers :: Query (UsersRow Expr)
activeUsers = do
  user <- each usersSchema
  where_ $ userStatus user ==. lit Enabled
  pure user

usersTable :: DbTable UsersRow
usersTable = DbTable usersSchema
  [ PK userId
  -- , AutoInc userId
  ]

data UserRole
  = Admin
  | Trustee
  | Uploader
  | MirrorClient
  deriving stock (Show, Read, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass DBEq
  deriving DBType via ReadShow UserRole

type UserRoleId = PrimaryKey UserRoleRow

data UserRoleRow f = UserRoleRow
  { userRoleId :: Column f UserRoleId
  , userRoleUserId :: Column f UserId
  , userRoleRole :: Column f UserRole
  , userRoleAssignedTime :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance Show (UserRoleRow Result)

userRolesSchema :: TableSchema (UserRoleRow Name)
userRolesSchema = TableSchema
  { name = "user_roles"
  , columns = UserRoleRow
      { userRoleId = "user_role_id"
      , userRoleUserId = "user_id"
      , userRoleRole = "role"
      , userRoleAssignedTime = "assigned_time"
      }
  }

userRolesTable :: DbTable UserRoleRow
userRolesTable = DbTable userRolesSchema
  [ PK userRoleId
  , AutoInc userRoleId
  , FK userRoleUserId usersSchema userId
  ]

type AuthTokenId = PrimaryKey UserAuthTokenRow

data UserAuthTokenRow f = UserAuthTokenRow
  { authTokenId :: Column f AuthTokenId
  , authTokenUserId :: Column f UserId
  , authTokenToken :: Column f AuthToken
  , authTokenDescription :: Column f (Maybe Text)
  , authTokenCreatedTime :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance Show (UserAuthTokenRow Result)

userAuthTokensSchema :: TableSchema (UserAuthTokenRow Name)
userAuthTokensSchema = TableSchema
  { name = "user_auth_tokens"
  , columns = UserAuthTokenRow
      { authTokenId = "id"
      , authTokenUserId = "user_id"
      , authTokenToken = "token"
      , authTokenDescription = "description"
      , authTokenCreatedTime = "created_time"
      }
  }


userAuthTokensTable :: DbTable UserAuthTokenRow
userAuthTokensTable = DbTable userAuthTokensSchema
  [ PK authTokenToken
  , FK authTokenUserId usersSchema userId
  ]
