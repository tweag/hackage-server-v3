module Hackage.Types where

import Data.Text (Text)
import Rel8 hiding (Enum)
import Data.Int (Int64)
import Data.ByteString (ByteString)
import Rel8.CreateTable

newtype UserId = UserId Int64
  deriving newtype (Eq, Ord, Show, DBType, DBEq, DBOrd, DBAutoInc)


data UserStatus = Enabled | Disabled | Deleted
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)
  deriving anyclass DBEq
  deriving DBType via ReadShow UserStatus

type DistroName = Text
type PackageName = Text
type UserName = Text
type Nonce = Text
type Tag = Text
type ReportId = Text
type Revision = Text


-- | A password hash. It actually contains the hash of the username, passowrd
-- and realm.
--
-- Hashed passwords are stored in the format
-- @md5 (username ++ ":" ++ realm ++ ":" ++ password)@. This format enables
-- us to use either the basic or digest HTTP authentication methods.
--
newtype PasswdHash = PasswdHash ByteString
  deriving newtype (Eq, Ord, Show, DBType, DBEq, DBOrd)

