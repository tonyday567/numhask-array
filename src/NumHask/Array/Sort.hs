{-# LANGUAGE RebindableSyntax #-}

-- |
module NumHask.Array.Sort
  ( orderV,
    orderByV,
  ) where

import NumHask.Prelude
import Data.Ord (comparing)
import Data.Vector.Algorithms.Intro (sortBy)
import Data.Vector.Unboxed (generate, modify)
import Data.Vector (Vector, unsafeIndex, convert)
import qualified Data.Vector as V

-- |
--
-- >>> λ> Sort.order (V.fromList [0..5::Int])
-- [0,1,2,3,4,5]
orderV :: (Ord a) => Vector a -> Vector Int
orderV a = idx
    where
    idx  = convert $ modify (sortBy comp) init0
    comp = comparing $ unsafeIndex a  -- comparing function
    init0 = generate (V.length a) id   -- [0..size - 1]

-- |
--
-- >>> λ> Sort.order (V.fromList [0..5::Int])
-- [0,1,2,3,4,5]
orderByV :: (Ord b) => (a -> b) -> Vector a -> Vector Int
orderByV c a = idx
    where
    idx  = convert $ modify (sortBy comp) init0
    comp = comparing $ c . unsafeIndex a  -- comparing function
    init0 = generate (V.length a) id   -- [0..size - 1]
