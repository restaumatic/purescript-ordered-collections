module Data.Map
  ( module Data.Map.Internal
  , keys
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Plus (class Plus)
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Map.Internal (Map, alter, catMaybes, checkValid, delete, empty, filter, filterKeys, filterWithKey, findMax, findMin, foldSubmap, fromFoldable, fromFoldableWith, fromFoldableWithIndex, insert, insertWith, isEmpty, isSubmap, lookup, lookupGE, lookupGT, lookupLE, lookupLT, member, pop, showTree, singleton, size, submap, toUnfoldable, toUnfoldableUnordered, union, unionWith, unions, intersection, intersectionWith, difference, update, values, mapMaybeWithKey, mapMaybe, any, anyWithKey)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord1)
import Data.Traversable (class Traversable)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Data.Set (Set, fromMap)

-- | The set of keys of the given map.
-- | See also `Data.Set.fromMap`.
keys :: forall k v. Map k v -> Set k
keys = fromMap <<< void
