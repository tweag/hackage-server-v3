{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans   #-}

module Hackage.Orphans where

import Servant.API
import Rel8 hiding (Enum)
import Distribution.Types.PackageId (PackageIdentifier(..))
import Distribution.Types.Version
import Distribution.Types.PackageName
import Test.QuickCheck
import Data.Text qualified as T
import Data.Int (Int64)
import Data.Text (Text)
import Distribution.Package qualified as Pkg
import Data.Functor.Contravariant
import Distribution.Parsec


instance Arbitrary PackageIdentifier where
  arbitrary = PackageIdentifier <$> arbitrary <*> arbitrary

instance DBType PackageName where
  typeInformation =
    let ti = typeInformation @Text
    in ti { encode = contramap (T.pack . Pkg.unPackageName) $ encode ti
          , decode = fmap (mkPackageName . T.unpack) $ decode ti
          }

instance DBEq PackageName
instance DBOrd PackageName

instance Arbitrary PackageName where
  arbitrary = mkPackageName <$> arbitrary

instance DBType Version where
  typeInformation =
    let ti = typeInformation @[Int64]
    in ti { encode = contramap (fmap fromIntegral . versionNumbers) $ encode ti
          , decode = fmap (mkVersion . fmap fromIntegral) $ decode ti
          }

instance DBEq Version
instance DBOrd Version

instance Arbitrary Version where
  arbitrary = mkVersion <$> fmap (fmap getNonNegative) arbitrary

instance FromHttpApiData PackageName where
  parseUrlPiece = fmap mkPackageName . parseUrlPiece

instance FromHttpApiData PackageIdentifier where
  parseUrlPiece = maybe (Left "Can't parse package identifier") Right . simpleParsec . T.unpack

