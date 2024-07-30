{-# LANGUAGE RebindableSyntax #-}

-- |
module NumHask.Array.Sort
  ( sortV,
    sortByV,
    orderV,
    orderByV,
  ) where

import NumHask.Prelude
import Data.Ord (comparing)
import Data.Vector.Algorithms.Intro (sortBy)
import Data.Vector.Unboxed (generate, modify)
import Data.Vector (Vector, unsafeIndex, convert)
import qualified Data.Vector as V

-- $setup
-- >>> :m -Prelude
-- >>> import Data.Vector qualified as V
-- >>> import Data.Ord (Down (..))
-- >>> :set -XDataKinds
-- >>> :set -XTypeFamilies
-- >>> :set -XFlexibleContexts
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude

-- | return the sorted array
--
-- >>> sortV (V.fromList [3,1,4,2,0,5::Int])
-- [0,1,2,3,4,5]
sortV :: (Ord a) => Vector a -> Vector a
sortV a = V.unsafeBackpermute a (orderV a)

-- | return the array sorted by the comparison function
--
-- >>> sortByV Down (V.fromList [3,1,4,2,0,5::Int])
-- [5,4,3,2,1,0]
sortByV :: (Ord b) => (a -> b) -> Vector a -> Vector a
sortByV c a = V.unsafeBackpermute a (orderByV c a)

-- | returns the indices of the elements in ascending order.
--
-- >>> orderV (V.fromList [0..5::Int])
-- [0,1,2,3,4,5]
orderV :: (Ord a) => Vector a -> Vector Int
orderV a = idx
    where
    idx  = convert $ modify (sortBy comp) init0
    comp = comparing $ unsafeIndex a  -- comparing function
    init0 = generate (V.length a) id   -- [0..size - 1]

-- | returns the indices of the elements in order given a comparison function.
--
-- >>> orderByV Down (V.fromList [0..5::Int])
-- [5,4,3,2,1,0]
orderByV :: (Ord b) => (a -> b) -> Vector a -> Vector Int
orderByV c a = idx
    where
    idx  = convert $ modify (sortBy comp) init0
    comp = comparing $ c . unsafeIndex a  -- comparing function
    init0 = generate (V.length a) id   -- [0..size - 1]
