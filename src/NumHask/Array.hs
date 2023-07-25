-- | Multi-dimensional arrays
module NumHask.Array
  ( -- * Imports
    -- $imports

    -- * Overview
    -- $overview
    module NumHask.Array.Shape,
    module NumHask.Array.Fixed,
  )
where

import NumHask.Array.Fixed
import NumHask.Array.Shape

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

-- $overview
--
-- 'Array's are higher-kinded numbers that can be indexed into with an @[Int]@. Higher-kinded numbers are things with a non-primitive type that we wish to use the usual numerical operators on: '+','-','*','/','abs','tan' and so on.
--
-- The design of numhask-array:
--
-- - allows shape to be specified at both the type and value level.
--
-- - provides operators at value and type level to help manipulate shapes.
--
-- - provides fixed and dynamic arrays with the same API.
--
