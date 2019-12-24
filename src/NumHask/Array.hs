{-# OPTIONS_GHC -Wall #-}

-- | Numbers that can be indexed into with an Int list.
--
--
module NumHask.Array
  ( -- * Imports
    --
    -- $imports
    --
    module NumHask.Array.Shape,
    module NumHask.Array.Fixed,
  ) where

import NumHask.Array.Shape
import NumHask.Array.Fixed

-- $imports
--
-- > import NumHask.Array
--
-- imports the fixed version of `NumHask.Array.Fixed.Array` and `Shape`
--
-- In many situations, where shape is being tracked or otherwise only known at runtime, a clear module arrangement is:
--
-- > import NumHask.Array.Shape
-- > import qualified NumHask.Array.Fixed as F
-- > import qualified NumHask.Array.Dynamic as D
--
-- A hmatrix instance of Array is also provided for performance purposes:
--
-- > import NumHask.Array.Shape
-- > import qualified NumHask.Array.HMatrix as H
--
