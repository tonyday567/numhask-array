numhask-array
===

[![Build Status](https://travis-ci.org/tonyday567/numhask.svg)](https://travis-ci.org/tonyday567/numhask) 
[![Hackage](https://img.shields.io/hackage/v/numhask-array.svg)](https://hackage.haskell.org/package/numhask-array) 

This package provides an interface into the numhask API, and both type and value level shape manipulation routines.

Usage
===

``` haskell
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE RebindableSyntax #-}
import NumHask.Prelude
import NumHask.Array
```

In situations where shape is only known at runtime, a clear module configuration is:

``` haskell
import NumHask.Array.Shape
import qualified NumHask.Array.Fixed as F
import qualified NumHask.Array.Dynamic as D
```

Performance
===

Performance experiments are located in [numhask-bench](https://github.com/tonyday567/numhask-bench). [numhask-hmatrix](https://github.com/tonyday567/numhask-hmatrix) provides a more performant and similar interface.


