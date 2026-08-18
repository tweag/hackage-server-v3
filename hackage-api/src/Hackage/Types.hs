{-# LANGUAGE OverloadedStrings #-}

module Hackage.Types
  ( module Hackage.Types
  , PackageName
  , PackageIdentifier(..)
  , PackageId
  , Version
  ) where

import Hackage.Objects (Schema(..))
import Data.Profunctor (lmap)
import Data.Schema qualified as S
import Data.Bifunctor (bimap)
import Data.Aeson (ToJSON, FromJSON)
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Functor.Contravariant
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Distribution.Package (PackageName, PackageIdentifier(..), PackageId)
import Distribution.Types.Version (Version)
import Distribution.Utils.MD5 (MD5, showMD5)
import GHC.Fingerprint (Fingerprint(..))
import GHC.Generics
import Numeric (readHex)
import Rel8 hiding (Enum)
import Rel8.Decoder (parseDecoder)
import Servant.API (ToHttpApiData(..), FromHttpApiData(..), MimeRender, PlainText)
import Test.QuickCheck


newtype UserId = UserId Int64
  deriving newtype
    ( Eq
    , Ord
    , Show
    , DBType
    , DBEq
    , DBOrd
    , ToJSON
    , FromJSON
    , Hashable
    , Arbitrary
    , ToHttpApiData
    , FromHttpApiData
    )
  deriving stock Data


data UserStatus = Enabled | Disabled | Deleted
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic, Data)
  deriving anyclass (DBEq, Hashable)
  deriving DBType via ReadShow UserStatus

instance Arbitrary UserStatus where
  arbitrary = elements $ enumFromTo minBound maxBound


newtype UserName = UserName Text
  deriving newtype
    ( Eq
    , Ord
    , Show
    , Hashable
    , DBEq
    , DBType
    , ToJSON
    , FromJSON
    , MimeRender PlainText
    , ToHttpApiData
    , FromHttpApiData
    )
  deriving stock Data

instance Arbitrary UserName where
  arbitrary = fmap (UserName . T.pack . getPrintableString) arbitrary


type Tag = Text
type SHA256Digest = Text
type TarballRevIx = Int64

newtype MetadataRevIx = MetadataRevIx
  { getMetadataRevix :: Int64
  }
  deriving newtype
    ( Eq
    , Ord
    , Show
    , Read
    , Enum
    , Bounded
    , DBEq
    , DBOrd
    , DBType
    , ToJSON
    , FromJSON
    , Arbitrary
    , ToHttpApiData
    , FromHttpApiData
    )


type role BlobId nominal
newtype BlobId a = BlobId
  { getBlobId :: MD5
  }
  deriving newtype (Eq, Ord, Show)
  deriving anyclass (DBEq, DBOrd)
  deriving (ToJSON, FromJSON) via Schema (BlobId a)

instance ToHttpApiData (BlobId a) where
  toUrlPiece = T.pack . showMD5 . getBlobId

instance FromHttpApiData (BlobId a) where
  parseUrlPiece = bimap T.pack coerce . parseMD5 . T.unpack

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

instance S.ToSchema (BlobId a) where
  schema
    = lmap (T.pack . showMD5 . getBlobId)
    $ S.parsedText "BlobId"
    $ coerce . parseMD5 . T.unpack

