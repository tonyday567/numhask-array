{-# OPTIONS_GHC -Wall #-}

-- | Higher-kinded numbers that can be represented with an [Int] index into elements.
--
module NumHask.Array
  ( -- * Shapes
    --
    -- $shape
    module NumHask.Array.Shape,

    -- * Numbers with a fixed shape
    --
    -- $fixed
    module NumHask.Array.Fixed,

    -- * Numbers with a dynamic shape
    --
    -- $dynamic

    -- * HMatrix API
    --
    -- $hmatrix

  ) where

import NumHask.Array.Shape
import NumHask.Array.Fixed

-- $shape
--
-- Using [`Int`] as the index for an array nicely represents the practical interests and constraints downstream of this high-level API: densely-packed numbers (reals or integrals), indexed and layered.

-- $fixed
--
-- A design principle of modern haskell (synonymous with ghc usage) is to push computation into compilation. `Representable` is an iconic example. `Shape` must ultimately be known at compile-time, but if it is, we can dispense with a large classes of runtime check which slow pipelines.  This tends to create computation where shape is abstracted and we fold algorithms in and out of structures.

-- $dynamic
--
-- | In many situations, where shape is being tracked or otherwise only known at runtime, a clear module arrangement is:
--
-- >>> import NumHask.Array.Shape
-- >>> import qualified NumHask.Array.Fixed as F
-- >>> import qualified NumHask.Array.Dynamic as D

-- $hmatrix
--
-- | hmatrix remains the speed king within haskell, and numhask-array is no exception. If speed matters then:
--
-- >>> import NumHask.Array.Shape
-- >>> import qualified NumHask.Array.HMatrix as H
--
-- will deliver the NumHask API over a HMatrix representation, and then the current fastest way to multiply matices in boiler-plate haskell.
--
-- FIXME: fill out Fixed API










