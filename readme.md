numhask-array
===

[![Build Status](https://travis-ci.org/tonyday567/numhask.svg)](https://travis-ci.org/tonyday567/numhask) 
[![Hackage](https://img.shields.io/hackage/v/numhask-array.svg)](https://hackage.haskell.org/package/numhask-array) 

Arrays are higher-kinded numbers that can be indexed into with an Int list. Higher-kinded numbers are things with a non-primitive type that we wish to use the usual numerical operators on (+,-,*,/,abs).

This is an experimental library that:
- allows shape to be specified at both the type and value level.
- provides operators at value and type level to help manipulate shapes.
- Provides fixed and dynamic arrays with the same API.

Performance experiments are located in [numhask-bench](https://github.com/tonyday567/numhask-bench)

Usefulness of the array language that results from this treatment is yet to be explored.

API of an array language
---

https://en.wikipedia.org/wiki/APL_(programming_language)

See http://hiperfit.dk/pdf/array14_final.pdf for context and a sketch of an intermediate typed array language effort.

The operators that result from using the Representable type - separation of size tracking at compile level, from computational at runtime - ends up looking like APL.

Matrix multiplication in APL is `+.x` and in numhask-array is `dot sum (*)`.  There is a slight increase in abstraction by explicitly exposing the fold in the algorithm, but the expressions are both very neat and abstracted away from the specialisation of multiplying matrices.

References
---

https://blog.plover.com/prog/apl-matrix-product.html

https://en.wikipedia.org/wiki/Tensor_contraction

https://en.wikipedia.org/wiki/Tensor_(intrinsic_definition)#Definition:_Tensor_Product_of_Vector_Spaces
