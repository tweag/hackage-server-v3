module Hackage.Types
  ( module Hackage.Types
  , PackageName
  , Version
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Functor.Contravariant
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Distribution.Package (PackageName)
import Distribution.Types.Version (Version)
import Distribution.Utils.MD5 (MD5, showMD5)
import GHC.Fingerprint (Fingerprint(..))
import Numeric (readHex)
import Rel8 hiding (Enum)
import Rel8.CreateTable
import Rel8.Decoder (parseDecoder)
import Test.QuickCheck (Arbitrary)


newtype UserId = UserId Int64
  deriving newtype
    ( Eq
    , Ord
    , Show
    , DBType
    , DBEq
    , DBOrd
    , DBAutoInc
    , ToJSON
    , FromJSON
    , Hashable
    , Arbitrary
    )


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
type SHA256Digest = Text
type TarballRevIx = Int64
type MetadataRevIx = Int64


type role BlobId nominal
newtype BlobId a = BlobId
  { getBlobId :: MD5
  }
  deriving newtype (Eq, Ord, Show)
  deriving anyclass (DBEq, DBOrd)

parseMD5 :: String -> Either String MD5
parseMD5 s = maybe (Left "Can't parse md5") Right $ do
  let (lo, hi) = splitAt 16 s
  a <- fmap fst $ listToMaybe $ readHex lo
  b <- fmap fst $ listToMaybe $ readHex hi
  pure $ Fingerprint a b

instance DBType (BlobId a) where
  typeInformation = do
    let ti = typeInformation @Text
    ti
      { encode = contramap (T.pack . showMD5 . getBlobId) $ encode ti
      , decode = parseDecoder (coerce . parseMD5 . T.unpack) $ decode ti
      }


-- | A password hash. It actually contains the hash of the username, passowrd
-- and realm.
--
-- Hashed passwords are stored in the format
-- @md5 (username ++ ":" ++ realm ++ ":" ++ password)@. This format enables
-- us to use either the basic or digest HTTP authentication methods.
--
newtype PasswdHash = PasswdHash ByteString
  deriving newtype (Eq, Ord, Show, DBType, DBEq, DBOrd)

