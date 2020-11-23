{-# OPTIONS_GHC -Wall #-}

-- | Multi-dimensional arrays for numhask.
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
-- === API of an array language
--
-- See <http://hiperfit.dk/pdf/array14_final.pdf> for context and a sketch of an intermediate typed array language effort.
--
-- The operators that result from using the 'Representable' type - separation of size tracking at compile level, from computational at runtime - ends up looking like [APL](https://en.wikipedia.org/wiki/APL_(programming_language\)).
--
-- Matrix multiplication in APL is @+.x@ and in numhask-array is @dot sum (*)@.  There is a slight increase in abstraction by explicitly exposing the fold in the algorithm, but the expressions are both very neat and abstracted away from the specialisation of multiplying matrices.
--
-- References:
--
-- <https://blog.plover.com/prog/apl-matrix-product.html>
--
-- <https://en.wikipedia.org/wiki/Tensor_contraction>
--
-- <https://en.wikipedia.org/wiki/Tensor_(intrinsic_definition)#Definition:_Tensor_Product_of_Vector_Spaces>
