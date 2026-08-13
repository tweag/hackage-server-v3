module MD5Spec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Hackage.Types
import Distribution.Utils.MD5 (md5, showMD5)
import Data.ByteString qualified as BS


spec :: Spec
spec = do
  prop "showMD5 . parseMD5 = id" $ \str -> do
    let m = md5 $ BS.pack str
    parseMD5 (showMD5 m) `shouldBe` Right m

