{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans     #-}

module ShrinkersSpec where

import Data.Set qualified as S
import Data.Set (Set)
import Data.Typeable
import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck
import Model

spec :: Spec
spec = modifyMaxShrinks (const 0) $ do
  shrinkProp @ModelHackage
  shrinkProp @ModelUser
  shrinkProp @ModelTarball
  shrinkProp @PathSeg
  shrinkProp @FileEntry
  sshrinkProp @ModelPackage
  sshrinkProp @ModelPkgInfo
  sshrinkProp @ModelUserRef
  sshrinkProp @ModelMetaRev


shrinkProp :: forall a. (Eq a, Arbitrary a, Show a, Typeable a) => Spec
shrinkProp =
  it ("shrink @(" <> show (typeRep (Proxy @a)) <> ") terminates") $
    forAll (arbitrary @a) $ \a ->
      shrink a `shouldNotSatisfy` elem a


sshrinkProp :: forall a s. (Ord s, Eq a, Arbitrary s, SomewhatArbitrary (Set s) a, Show s, Show a, Typeable a) => Spec
sshrinkProp =
  it ("shrink @(" <> show (typeRep (Proxy @a)) <> ") terminates") $
    forAll (arbitrary @(NonEmptyList s)) $ \(NonEmpty s) -> do
      let ss = S.fromList s
      forAll (sarbitrary ss) $ \(a :: a) ->
        sshrink ss a `shouldNotSatisfy` elem a


instance Arbitrary ModelUserRef where
  arbitrary = ModelUserRef <$> arbitrary <*> arbitrary
