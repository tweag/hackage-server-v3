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
  ) where

import Data.Time (UTCTime)
import GHC.Generics (Generic)
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
  , userStatus :: Column f UserStatus
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
      , userStatus = "user_status"
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
  ]

