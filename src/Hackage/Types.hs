module Hackage.Types
  ( module Hackage.Types
  , PackageName
  , Version
  ) where

import Data.Functor.Contravariant
import Data.Text (Text)
import Rel8 hiding (Enum)
import Data.Int (Int64)
import Data.ByteString (ByteString)
import Rel8.CreateTable
import Distribution.Package (PackageName, mkPackageName)
import Distribution.Types.Version (Version, mkVersion, versionNumbers)
import qualified Data.Text as T
import qualified Distribution.Package as Pkg

newtype UserId = UserId Int64
  deriving newtype (Eq, Ord, Show, DBType, DBEq, DBOrd, DBAutoInc)


data UserStatus = Enabled | Disabled | Deleted
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)
  deriving anyclass DBEq
  deriving DBType via ReadShow UserStatus

type DistroName = Text
type UserName = Text
type Nonce = Text
type Tag = Text
type ReportId = Text
type Revision = Text
type BlobId = Text
type SHA256Digest = Text
type TarballRevIx = Int64
type MetadataRevIx = Int64


-- | A password hash. It actually contains the hash of the username, passowrd
-- and realm.
--
-- Hashed passwords are stored in the format
-- @md5 (username ++ ":" ++ realm ++ ":" ++ password)@. This format enables
-- us to use either the basic or digest HTTP authentication methods.
--
newtype PasswdHash = PasswdHash ByteString
  deriving newtype (Eq, Ord, Show, DBType, DBEq, DBOrd)


instance DBType PackageName where
  typeInformation =
    let ti = typeInformation @Text
    in ti { encode = contramap (T.pack . Pkg.unPackageName) $ encode ti
          , decode = fmap (mkPackageName . T.unpack) $ decode ti
          }

instance DBEq PackageName
instance DBOrd PackageName

instance DBType Version where
  typeInformation =
    let ti = typeInformation @[Int64]
    in ti { encode = contramap (fmap fromIntegral . versionNumbers) $ encode ti
          , decode = fmap (mkVersion . fmap fromIntegral) $ decode ti
          }

instance DBEq Version
instance DBOrd Version
