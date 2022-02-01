numhask-array
===

[![Hackage](https://img.shields.io/hackage/v/numhask-array.svg)](https://hackage.haskell.org/package/numhask-array)
[![Build Status](https://github.com/tonyday567/numhask-array/workflows/haskell-ci/badge.svg)](https://github.com/tonyday567/numhask-array/actions?query=workflow%3Ahaskell-ci)

This package provides an interface into the numhask API, and both type and value level shape manipulation routines.

Usage
===

``` haskell
{-# LANGUAGE RebindableSyntax #-}
import NumHask.Prelude
import NumHask.Array
```

In situations where shape is only known at run-time, a clear module configuration is:

``` haskell
import NumHask.Array.Shape
import qualified NumHask.Array.Fixed as F
import qualified NumHask.Array.Dynamic as D
```


