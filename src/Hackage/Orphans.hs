{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans   #-}

module Hackage.Orphans where

import Data.ByteString (StrictByteString)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Functor.Contravariant
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Distribution.Compat.NonEmptySet (NonEmptySet)
import Distribution.Compat.NonEmptySet qualified as NES
import Distribution.Compat.Prelude (NonEmpty(..))
import Distribution.Package qualified as Pkg
import Distribution.Parsec
import Distribution.Pretty qualified as Pretty
import Distribution.Types.Dependency
import Distribution.Types.LibraryName
import Distribution.Types.PackageId (PackageIdentifier(..))
import Distribution.Types.PackageName
import Distribution.Types.UnqualComponentName
import Distribution.Types.Version
import Distribution.Types.VersionRange
import Rel8 hiding (Enum)
import Servant.API
import Test.QuickCheck

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

instance Arbitrary Dependency where
  arbitrary = Dependency <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary VersionRange where
  arbitrary = do
    let small = [ pure anyVersion
                , pure noVersion
                , thisVersion <$> arbitrary
                , notThisVersion <$> arbitrary
                , laterVersion <$> arbitrary
                , earlierVersion <$> arbitrary
                , orLaterVersion <$> arbitrary
                , orEarlierVersion <$> arbitrary
                , withinVersion <$> arbitrary
                , majorBoundVersion <$> arbitrary
                ]
    sized $ \n ->
      case n <= 1 of
        True -> oneof small
        False -> oneof $ small <>
          [ unionVersionRanges <$> resize (div n 2) arbitrary <*> resize (div n 2) arbitrary
          , intersectVersionRanges <$> resize (div n 2) arbitrary <*> resize (div n 2) arbitrary
          ]

instance (Ord a, Arbitrary a) => Arbitrary (NonEmptySet a) where
  arbitrary = fmap NES.fromNonEmpty $ (:|) <$> arbitrary <*> arbitrary

instance Arbitrary LibraryName where
  arbitrary = oneof
    [ pure LMainLibName
    , fmap LSubLibName arbitrary
    ]

instance Arbitrary UnqualComponentName where
  arbitrary = fmap mkUnqualComponentName arbitrary

instance DBEq Version
instance DBOrd Version

instance Arbitrary Version where
  arbitrary = mkVersion <$> fmap (fmap getNonNegative) arbitrary

instance FromHttpApiData PackageName where
  parseUrlPiece = fmap mkPackageName . parseUrlPiece

instance ToHttpApiData PackageName where
  toUrlPiece = T.pack . unPackageName

instance FromHttpApiData PackageIdentifier where
  parseUrlPiece = maybe (Left "Can't parse package identifier") Right . simpleParsec . T.unpack

instance ToHttpApiData PackageIdentifier where
  toUrlPiece = T.pack . Pretty.prettyShow

instance MimeRender PlainText UTCTime where
  mimeRender _ = LBS.pack . show

instance MimeRender PlainText StrictByteString where
  mimeRender _ = LBS.fromStrict

instance Arbitrary UTCTime where
  arbitrary = UTCTime <$> arbitrary <*> arbitrary

instance Arbitrary Day where
  arbitrary = fromSundayStartWeek <$> fmap (+ 2000) arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary DiffTime where
  arbitrary = realToFrac <$> arbitrary @Double

