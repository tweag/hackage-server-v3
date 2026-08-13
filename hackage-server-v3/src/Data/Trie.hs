{-# LANGUAGE OverloadedStrings #-}

module Data.Trie
  ( Trie(..)
  , foldTrieMap
  , pathToTrie
  , nestTrie
  , flattenTrie
  , TrieCmd(..)
  ) where

import Data.Aeson
import Data.Map.Monoidal (MonoidalMap)
import Data.Map.Monoidal qualified as MM
import GHC.Generics (Generic, Generically(..))
import Test.QuickCheck

-- | A @k@ indexed trie. The trie itself doesn't contain any concrete values,
-- so we treat any leaf as a value.
newtype Trie k = Trie
  { unTrie :: MonoidalMap k (Trie k)
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Semigroup, Monoid) via Generically (Trie k)


-- | Fold the full paths down to leaves of a 'Trie'.
foldTrieMap :: Monoid m => ([k] -> m) -> Trie k -> m
foldTrieMap f t0 = go [] t0
  where
    go path (Trie children)
      | MM.null children =
        case null path of
          True -> mempty
          False -> f path
      | otherwise
      = foldMap
          (\(segment, child) -> go (path <> [segment]) child) $
          MM.toList children

instance (ToJSONKey k) => ToJSON (Trie k) where
  toJSON (Trie s) =
    object
      [ "children" .= s
      ]

instance (Ord k, Arbitrary k) => Arbitrary (Trie k) where
  arbitrary = sized $ \n -> do
    let small =
          [ pure mempty
          , fmap Trie $ MM.singleton <$> arbitrary <*> pure mempty
          ]
    case n <= 1 of
      True -> oneof small
      False -> oneof $ small <>
        [ nestTrie <$> arbitrary <*> scale (subtract 1) arbitrary
        , (<>) <$> scale (`div` 2) arbitrary <*> scale (`div` 2) arbitrary
        ]


-- | Add a common prefix to all elements in a 'Trie'.
nestTrie :: k -> Trie k -> Trie k
nestTrie k = Trie . MM.singleton k


-- | Convert a path of keys into a singleton 'Trie'.
pathToTrie :: Ord k => [k] -> Trie k
pathToTrie v = foldr nestTrie mempty v


-- | Stack machine instructions for constructing a 'Trie'. This can be useful
-- in domains like EDE which don't support arbitrary recursion, but in which
-- we'd like to render a trie nevertheless.
data TrieCmd segment path
  = TriePush path segment
  | TrieVal
      path
      -- ^ Accumulated path to get here
      segment
      -- ^ Segment
  | TriePop
  deriving stock (Eq, Ord, Show, Generic, Functor)


-- | 'TrieCmd's are encoded as S-expressions.
instance (ToJSON segment, ToJSON path) => ToJSON (TrieCmd segment path) where
  toJSON (TriePush path seg) = toJSON ["push", toJSON path, toJSON seg]
  toJSON (TrieVal path seg) = toJSON ["item", toJSON path, toJSON seg]
  toJSON (TriePop) = toJSON [id @String "pop"]


-- | Given a 'Trie', compute the sequence of 'TrieCmd's which build it.
flattenTrie :: forall v. Trie v -> [TrieCmd v [v]]
flattenTrie = go []
  where
    go :: [v] -> Trie v -> [TrieCmd v [v]]
    go path
      = foldMap (uncurry $ flattenChild path)
      . MM.toList
      . unTrie

    flattenChild :: [v] -> v -> Trie v -> [TrieCmd v [v]]
    flattenChild path segment child@(Trie children)
      | MM.null children
      = pure $ TrieVal path segment
      | otherwise
      = TriePush path segment : go (path <> [segment]) child <> [TriePop]

