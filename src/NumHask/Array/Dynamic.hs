{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Arrays with a dynamic shape
module NumHask.Array.Dynamic
  ( -- * Dynamic Arrays
    --
    -- $array
    Array(..),
    fromFlatList,

    -- * Operators
    --
    -- $operators
    reshape,
    transpose,
    diag,
    selects,
    selectsExcept,
    folds,
    extracts,
    joins,
    maps,
    concatenate,
    insert,
    append,
    reorder,
    expand,
    contract,
    dot,
    slice,
    squeeze,
    singleton,
    ident,
    -- * Scalar
    --
    -- $scalar
    fromScalar,
    toScalar,
    -- * Matrix
    --
    -- $matrix
    col,
    row,
    mmult,
  )where

import qualified Data.Vector as V
import GHC.Show (Show (..))
import NumHask.Array.Shape
import NumHask.Prelude as P hiding (product, transpose)
import Data.List ((!!))

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XOverloadedLists
-- >>> :set -XTypeFamilies
-- >>> :set -XFlexibleContexts
-- >>> let s = fromFlatList [] [1] :: Array Int
-- >>> let a = fromFlatList [2,3,4] [1..24] :: Array Int
-- >>> let v = fromFlatList [3] [1,2,3] :: Array Int
-- >>> let m = fromFlatList [3,4] [0..11] :: Array Int

-- | a multidimensional array with a value-level shape
--
-- >>> a
-- [[[1, 2, 3, 4],
--   [5, 6, 7, 8],
--   [9, 10, 11, 12]],
--  [[13, 14, 15, 16],
--   [17, 18, 19, 20],
--   [21, 22, 23, 24]]]
data Array a
  = Array {shape :: [Int], unArray :: V.Vector a}
  deriving (Eq, Ord, NFData, Generic)

instance Functor Array where
  fmap f (Array s a) = Array s (V.map f a)

instance Foldable Array where
  foldr x a (Array _ v) = V.foldr x a v

instance Traversable Array where
  traverse f (Array s v) =
    fromFlatList s <$> traverse f (toList v)

instance (Show a) => Show (Array a) where
  show a@(Array l _) = go (length l) a
    where
      go n a'@(Array l' m) =
        case length l' of
          0 -> maybe (throw (NumHaskException "empty scalar")) GHC.Show.show (head m)
          1 -> "[" ++ intercalate ", " (GHC.Show.show <$> V.toList m) ++ "]"
          x ->
            "["
              ++ intercalate
                (",\n" ++ replicate (n - x + 1) ' ')
                (go n <$> toFlatList (extracts [0] a'))
              ++ "]"

-- * conversions

-- | convert from a list
--
-- >>> fromFlatList [2,3,4] [1..24] == a
-- True
fromFlatList :: [Int] -> [a] -> Array a
fromFlatList ds l = Array ds $ V.fromList $ take (size ds) l

-- | convert to a flat list.
--
-- >>> toFlatList a == [1..24]
-- True
toFlatList :: Array a -> [a]
toFlatList (Array _ v) = V.toList v

-- | extract an element at index /i/
--
-- >>> index a [1,2,3]
-- 24
index :: () => Array a -> [Int] -> a
index (Array s v) i = V.unsafeIndex v (flatten s i)

-- | tabulate an array with a generating function
--
-- >>> tabulate [2,3,4] ((1+) . flatten [2,3,4]) == a
-- True
tabulate :: () => [Int] -> ([Int] -> a) -> Array a
tabulate ds f = Array ds . V.generate (size ds) $ (f . shapen ds)

-- | reshape an array (with the same number of elements)
--
-- >>> reshape [4,3,2] a
-- [[[1, 2],
--   [3, 4],
--   [5, 6]],
--  [[7, 8],
--   [9, 10],
--   [11, 12]],
--  [[13, 14],
--   [15, 16],
--   [17, 18]],
--  [[19, 20],
--   [21, 22],
--   [23, 24]]]
reshape ::
  [Int] ->
  Array a ->
  Array a
reshape s a = tabulate s (index a . shapen (shape a) . flatten s)

-- | reverse indices eg transposes the element /Aijk/ to /Akji/
--
-- >>> index (transpose a) [1,0,0] == index a [0,0,1]
-- True
transpose :: Array a -> Array a
transpose a = tabulate (reverse $ shape a) (index a . reverse)

-- |
--
-- >>> ident [3,2]
-- [[1, 0],
--  [0, 1],
--  [0, 0]]
ident :: (Num a) => [Int] -> Array a
ident ds = tabulate ds (bool 0 1 . isDiag)
  where
    isDiag [] = True
    isDiag [_] = True
    isDiag [x, y] = x == y
    isDiag (x : y : xs) = x == y && isDiag (y : xs)

-- | extract the diagonal
--
-- >>> diag (ident [3,2])
-- [1, 1]
diag ::
  Array a ->
  Array a
diag a = tabulate [NumHask.Array.Shape.minimum (shape a)] go
  where
    go [] = throw (NumHaskException "Rank Underflow")
    go (s' : _) = index a (replicate (rank (shape a)) s')

-- |
-- >>> singleton [3,2] one
-- [[1, 1],
--  [1, 1],
--  [1, 1]]
singleton :: [Int] -> a -> Array a
singleton ds a = tabulate ds (const a)

-- | /selects ds ps a/ select from /a/, elements along /ds/ dimensions at positions /ps/
--
-- >>> let s = selects [0,1] [1,1] a
-- >>> s
-- [17, 18, 19, 20]
selects ::
  [Int] ->
  [Int] ->
  Array a ->
  Array a
selects ds i a = tabulate (dropIndexes (shape a) ds) go
  where
    go s = index a (addIndexes s ds i)

-- | select an index /except/ along dimensions
--
-- >>> let s = selectsExcept [2] [1,1] a
-- >>> s
-- [17, 18, 19, 20]
selectsExcept ::
  [Int] ->
  [Int] ->
  Array a ->
  Array a
selectsExcept ds i a = selects (exclude (rank (shape a)) ds) i a

-- | fold along specified dimensions
--
-- >>> folds sum [1] a
-- [68, 100, 132]
folds ::
  (Array a -> b) ->
  [Int] ->
  Array a ->
  Array b
folds f ds a = tabulate (takeIndexes (shape a) ds) go
  where
    go s = f (selects ds s a)

-- | extracts dimensions to an outer layer
--
-- >>> let e = extracts [1,2] a
-- >>> shape <$> extracts [0] a
-- [[3,4], [3,4]]
extracts ::
  [Int] ->
  Array a ->
  Array (Array a)
extracts ds a = tabulate (takeIndexes (shape a) ds) go
  where
    go s = selects ds s a

-- | extracts /except/ dimensions to an outer layer
--
-- >>> let e = extractsExcept [1,2] a
-- >>> shape <$> extracts [0] a
-- [[3,4], [3,4]]
extractsExcept ::
  [Int] ->
  Array a ->
  Array (Array a)
extractsExcept ds a = extracts (exclude (rank (shape a)) ds) a

-- | join inner and outer dimension layers
--
-- >>> let e = extracts [1,0] a
-- >>> let j = joins [1,0] e
-- >>> a == j
-- True
joins ::
  [Int] ->
  Array (Array a) ->
  Array a
joins ds a = tabulate (addIndexes si ds so) go
  where
    go s = index (index a (takeIndexes s ds)) (dropIndexes s ds)
    so = shape a
    si = shape (index a (replicate (rank so) 0))

-- | maps along specified dimensions
--
-- >>> shape $ maps (transpose) [1] a
-- [4,3,2]
maps ::
  (Array a -> Array b) ->
  [Int] ->
  Array a ->
  Array b
maps f ds a = joins ds (fmap f (extracts ds a))

-- | concatenate along a dimension
--
-- >>> shape $ concatenate 1 a a
-- [2,6,4]
concatenate ::
  Int ->
  Array a ->
  Array a ->
  Array a
concatenate d a0 a1 = tabulate (concatenate' d (shape a0) (shape a1)) go
  where
    go s =
      bool
        (index a0 s)
        ( index
            a1
            ( addIndex
                (dropIndex s d)
                d
                ((s !! d) - (ds0 !! d))
            )
        )
        ((s !! d) >= (ds0 !! d))
    ds0 = shape a0

-- | /insert d i/ insert along the dimension /d/ at position /i/
--
-- >>> insert 2 0 a (fromFlatList [2,3] [100..105])
-- [[[100, 1, 2, 3, 4],
--   [101, 5, 6, 7, 8],
--   [102, 9, 10, 11, 12]],
--  [[103, 13, 14, 15, 16],
--   [104, 17, 18, 19, 20],
--   [105, 21, 22, 23, 24]]]
insert ::
  Int ->
  Int ->
  Array a ->
  Array a ->
  Array a
insert d i a b = tabulate (incAt d (shape a)) go
  where
    go s
      | s !! d == i = index b (dropIndex s d)
      | s !! d < i = index a s
      | otherwise = index a (decAt d s)

-- | insert along a dimension at the end
--
-- >>> append 2 a (fromFlatList [2,3] [100..105])
-- [[[1, 2, 3, 4, 100],
--   [5, 6, 7, 8, 101],
--   [9, 10, 11, 12, 102]],
--  [[13, 14, 15, 16, 103],
--   [17, 18, 19, 20, 104],
--   [21, 22, 23, 24, 105]]]
append ::
  Int ->
  Array a ->
  Array a ->
  Array a
append d a b = insert d (dimension (shape a) d) a b

-- | change the order of dimensions
--
-- >>> let r = reorder [2,0,1] a
-- >>> r
-- [[[1, 5, 9],
--   [13, 17, 21]],
--  [[2, 6, 10],
--   [14, 18, 22]],
--  [[3, 7, 11],
--   [15, 19, 23]],
--  [[4, 8, 12],
--   [16, 20, 24]]]
reorder ::
  [Int] ->
  Array a ->
  Array a
reorder ds a = tabulate (reorder' (shape a) ds) go
  where
    go s = index a (addIndexes [] ds s)

-- | product two arrays using the supplied binary function
-- If the function is multiply, and the arrays are tensors,
-- then this can be interpreted as a tensor product.
--
-- https://en.wikipedia.org/wiki/Tensor_product
--
-- The concept of a tensor product is a dense crossroad, and a complete treatment is elsewhere.  To quote:
-- ... the tensor product can be extended to other categories of mathematical objects in addition to vector spaces, such as to matrices, tensors, algebras, topological vector spaces, and modules. In each such case the tensor product is characterized by a similar universal property: it is the freest bilinear operation. The general concept of a "tensor product" is captured by monoidal categories; that is, the class of all things that have a tensor product is a monoidal category.
--
-- >>> expand (*) v v
-- [[1, 2, 3],
--  [2, 4, 6],
--  [3, 6, 9]]
expand ::
  (a -> b -> c) ->
  Array a ->
  Array b ->
  Array c
expand f a b = tabulate ((++) (shape a) (shape b)) (\i -> f (index a (take r i)) (index b (drop r i)))
  where
    r = rank (shape a)

-- | contract an array by applying the supplied (folding) function on diagonal elements of the dimensions.
--
-- This generalises a tensor contraction by allowing the number of contracting diagonals to be other than 2, and allowing another binary other than addition
--
-- >>> let b = fromFlatList [2,3] [1..6] :: Array Int
-- >>> contract sum [1,2] (expand (*) b (transpose b))
-- [[14, 32],
--  [32, 77]]
contract ::
  (Array a -> b) ->
  [Int] ->
  Array a ->
  Array b
contract f xs a = f . diag <$> extractsExcept xs a

-- | a generalisation of a dot operation, which is a multiplicative expansion of two arrays and sum contraction along the middle two dimensions.
--
-- dot sum (*) on two matrices is known as matrix multiplication
--
-- >>> let b = fromFlatList [2,3] [1..6] :: Array Int
-- >>> dot sum (*) b (transpose b)
-- [[14, 32],
--  [32, 77]]
--
-- dot sum (*) on two vectors is known as the inner product
--
-- >>> let v = fromFlatList [3] [1..3] :: Array Int
-- >>> dot sum (*) v v
-- 14
--
-- dot sum (*) m v on a matrix and a vector is matrix-vector multiplication
-- Note that an `Array Int` with shape [3] is neither a row vector nor column vector. `dot` is not turning the vector into a matrix and then using matrix multiplication.
--
-- >>> dot sum (*) v b
-- [9, 12, 15]
--
-- >>> dot sum (*) b v
-- [14, 32]
dot ::
  (Array c -> d) ->
  (a -> b -> c) ->
  Array a ->
  Array b ->
  Array d
dot f g a b = contract f [rank sa - 1, rank sa] (expand g a b)
  where
    sa = shape a

-- | select elements along every dimension
--
-- >>> let s = slice [[0,1],[0,2],[1,2]] a
-- >>> s
-- [[[2, 3],
--   [10, 11]],
--  [[14, 15],
--   [22, 23]]]
--
slice ::
  [[Int]] ->
  Array a ->
  Array a
slice pss a = tabulate (ranks pss) go
  where
    go s = index a (zipWith (!!) pss s)

-- | remove singleton dimensions
--
-- >>> let a' = fromFlatList [2,1,3,4,1] [1..24] :: Array Int
-- >>> shape $ squeeze a'
-- [2,3,4]
squeeze ::
  Array a ->
  Array a
squeeze (Array s x) = Array (squeeze' s) x

-- * scalar specializations

-- | <https://en.wikipedia.org/wiki/Scalarr_(mathematics) Wiki Scalar>
--
-- An /Array/ with shape [] despite being a Scalar is nevertheless a one-element vector under the hood.

-- | unwrapping scalars is probably a performance bottleneck
--
-- >>> let s = fromFlatList [] [3] :: Array Int
-- >>> fromScalar s
-- 3
fromScalar :: Array a -> a
fromScalar a = index a ([] :: [Int])

-- | convert a number to a scalar
--
-- >>> :t toScalar 2
-- toScalar 2 :: Num a => Array a
toScalar :: a -> Array a
toScalar a = fromFlatList [] [a]

-- * matrix specializations
-- | extract specialised to a matrix
--
-- >>> row 1 m
-- [4, 5, 6, 7]
row :: Int -> Array a -> Array a
row i (Array s a) = Array [n] $ V.slice (i * n) n a
  where
    [_,n] = s

-- | extract specialised to a matrix
--
-- >>> col 1 m
-- [1, 5, 9]
col :: Int -> Array a -> Array a
col i (Array s a) = Array [m] $ V.generate m (\x -> V.unsafeIndex a (i + x * n))
  where
    [m,n] = s

-- | matrix multiplication
--
-- This is dot sum (*) specialised to matrices
--
-- >>> let a = fromFlatList [2,2] [1, 2, 3, 4] :: Array Int
-- >>> let b = fromFlatList [2,2] [5, 6, 7, 8] :: Array Int
-- >>> a
-- [[1, 2],
--  [3, 4]]
--
-- >>> b
-- [[5, 6],
--  [7, 8]]
--
-- >>> mmult a b
-- [[19, 22],
--  [43, 50]]
mmult ::
  (Ring a) =>
  Array a ->
  Array a ->
  Array a
mmult (Array sx x) (Array sy y) = tabulate [m,n] go
  where
    go [] = throw (NumHaskException "Needs two dimensions")
    go [_] = throw (NumHaskException "Needs two dimensions")
    go (i : j : _) = sum $ V.zipWith (*) (V.slice (fromIntegral i * k) k x) (V.generate k (\x' -> y V.! (fromIntegral j + x' * n)))
    [m,k] = sx
    [_,n] = sy
{-# INLINE mmult #-}


