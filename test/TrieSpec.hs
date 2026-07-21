module TrieSpec where

import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Data.Trie
import Test.Hspec
import Test.Hspec.QuickCheck
import Data.List (sort)

spec :: Spec
spec = do
  prop "foldTrieMap id . pathToTrie = id" $ \(ks :: [Int]) ->
    foldTrieMap id (pathToTrie ks) `shouldBe` ks

  prop "foldTrieMap is a monoid homomorphism when there are no overlaps" $ \(p :: [Int]) (as :: [Int]) (bs :: [Int]) -> do
    let t1 = p <> (0 : as)
        t2 = p <> (1 : bs)
    foldTrieMap
        S.singleton
        (pathToTrie t1 <> pathToTrie t2)
      `shouldBe` (S.singleton t1 <> S.singleton t2)

  prop "foldTrieMap agrees with the items of flattenTrie" $ \(t :: Trie Int) -> do
    S.fromList (mapMaybe cmdLeaves $ flattenTrie t) `shouldBe` foldTrieMap S.singleton t

  prop "flattenTrie produces items in ascending order" $ \(t :: Trie Int) -> do
    let leaves = mapMaybe cmdLeaves $ flattenTrie t
    leaves `shouldBe` sort leaves

  prop "foldMap pathToTrie . foldTrieMap pure == id" $ \(t :: Trie Int) -> do
    foldMap pathToTrie (foldTrieMap S.singleton t) `shouldBe` t


cmdLeaves :: TrieCmd k [k] -> Maybe [k]
cmdLeaves (TrieVal path segment) = pure $ path <> [segment]
cmdLeaves TriePush{} = Nothing
cmdLeaves TriePop = Nothing

