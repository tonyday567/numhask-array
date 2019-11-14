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
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | Arrays with a dynamic shape
module NumHask.Array.Dynamic where

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
-- >>> let s = fromFlatList [] [1] :: DArray Int
-- >>> let a = fromFlatList [2,3,4] [1..24] :: DArray Int
-- >>> let v = fromFlatList [3] [1,2,3] :: DArray Int
-- >>> let m = fromFlatList [3,4] [0..11] :: DArray Int

-- | a multidimensional array with a value-level shape
--
-- >>> a
-- [[[1, 2, 3, 4],
--   [5, 6, 7, 8],
--   [9, 10, 11, 12]],
--  [[13, 14, 15, 16],
--   [17, 18, 19, 20],
--   [21, 22, 23, 24]]]
data DArray a
  = DArray {shape :: [Int], unArray :: V.Vector a}
  deriving (Eq, Ord, NFData, Generic)

instance Functor DArray where
  fmap f (DArray s a) = DArray s (V.map f a)

instance Foldable DArray where
  foldr x a (DArray _ v) = V.foldr x a v

instance Traversable DArray where
  traverse f (DArray s v) =
    fromFlatList s <$> traverse f (toList v)

instance (Show a) => Show (DArray a) where
  show a@(DArray l _) = go (length l) a
    where
      go n a'@(DArray l' m) =
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
fromFlatList :: [Int] -> [a] -> DArray a
fromFlatList ds l = DArray ds $ V.fromList $ take (size ds) l

-- | convert to a flat list.
-- >>> toFlatList a == [1..24]
-- True
toFlatList :: DArray a -> [a]
toFlatList (DArray _ v) = V.toList v

-- | extract an element at index /i/
--
-- >>> index a [1,2,3]
-- 24
index :: () => DArray a -> [Int] -> a
index (DArray s v) i = V.unsafeIndex v (flatten s i)

-- | tabulate an array with a generating function
--
-- >>> tabulate [2,3,4] ((1+) . flatten [2,3,4]) == a
-- True
tabulate :: () => [Int] -> ([Int] -> a) -> DArray a
tabulate ds f = DArray ds . V.generate (size ds) $ (f . shapen ds)

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
  DArray a ->
  DArray a
reshape s a = tabulate s (index a . shapen (shape a) . flatten s)

-- | reverse indices eg transposes the element /Aijk/ to /Akji/
--
-- >>> index (transpose a) [1,0,0] == index a [0,0,1]
-- True
transpose :: DArray a -> DArray a
transpose a = tabulate (shape a) (index a . reverse)

-- |
--
-- >>> ident [3,2]
-- [[1, 0],
--  [0, 1],
--  [0, 0]]
ident :: (Num a) => [Int] -> DArray a
ident ds = tabulate ds (bool 0 1 . isDiag)
  where
    isDiag [] = True
    isDiag [_] = True
    isDiag [x, y] = x == y
    isDiag (x : y : xs) = x == y && isDiag (y : xs)

-- | extract the diagonal
--
-- >>> diag (ident [3,2])
-- [[1, 1]]
diag ::
  DArray a ->
  DArray a
diag a = tabulate [1, NumHask.Array.Shape.minimum (shape a)] go
  where
    go [] = throw (NumHaskException "Rank Underflow")
    go (s' : _) = index a (replicate (rank (shape a)) s')

-- |
-- >>> singleton [3,2] one
-- [[1, 1],
--  [1, 1],
--  [1, 1]]
singleton :: [Int] -> a -> DArray a
singleton ds a = tabulate ds (const a)

-- | /selects ds ps a/ select from /a/, elements along /ds/ dimensions at positions /ps/
--
-- >>> let s = selects [0,1] [1,1] a
-- >>> s
-- [17, 18, 19, 20]
selects ::
  [Int] ->
  [Int] ->
  DArray a ->
  DArray a
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
  DArray a ->
  DArray a
selectsExcept ds i a = selects (exclude (rank (shape a)) ds) i a

-- | fold along specified dimensions
--
-- >>> folds sum [1] a
-- [68, 100, 132]
folds ::
  (DArray a -> b) ->
  [Int] ->
  DArray a ->
  DArray b
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
  DArray a ->
  DArray (DArray a)
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
  DArray a ->
  DArray (DArray a)
extractsExcept ds a = extracts (exclude (rank (shape a)) ds) a

-- | join inner and outer dimension layers
--
-- >>> let e = extracts [1,0] a
-- >>> let j = joins [1,0] e
-- >>> a == j
-- True
joins ::
  [Int] ->
  DArray (DArray a) ->
  DArray a
joins ds a = tabulate (addIndexes si ds so) go
  where
    go s = index (index a (takeIndexes s ds)) (dropIndexes s ds)
    so = shape a
    si = shape (index a (replicate (rank so) 0))

-- | maps along specified dimensions
--
-- >>> shape $ maps (transpose) [1] a
-- [4, 3, 2]
maps ::
  (DArray a -> DArray b) ->
  [Int] ->
  DArray a ->
  DArray b
maps f ds a = joins ds (fmap f (extracts ds a))

-- | concatenate along a dimension
--
-- >>> shape $ concatenate 1 a a
-- [2, 6, 4]
concatenate ::
  Int ->
  DArray a ->
  DArray a ->
  DArray a
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

-- | /insert d i/ insert along the dimension /d/ at position /i/
--
-- >>> insert 2 0 a ([100..105])
-- [[[100, 1, 2, 3, 4],
--   [101, 5, 6, 7, 8],
--   [102, 9, 10, 11, 12]],
--  [[103, 13, 14, 15, 16],
--   [104, 17, 18, 19, 20],
--   [105, 21, 22, 23, 24]]]
insert ::
  [Int] ->
  [Int] ->
  DArray a ->
  DArray a ->
  DArray a
insert d i a b = tabulate go
  where
    go s
      | s !! d == i = index b (dropIndex s d)
      | s !! d < i = index a s
      | otherwise = index a (decAt d s)

-- | insert along a dimension at the end
--
-- >>>  :t append 0 a a
--
append ::
  [Int] ->
  DArray a ->
  DArray a ->
  DArray a
append d = insert d (dimension s d - 1)

-- | change the order of dimensions
--
-- >>> let r = reorder [2,0,1] a
-- >>> r
--
reorder ::
  [Int] ->
  DArray a ->
  DArray a
reorder ds a = tabulate (reorder' s ds) go
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
  DArray a ->
  DArray b ->
  DArray c
expand f a b = tabulate ((++) (shape a) (shape b)) (\i -> f (index a (take r i)) (index b (drop r i)))
  where
    r = rank (shape a)

-- | contract an array by applying the supplied (folding) function on diagonal elements of the dimensions.
--
-- This generalises a tensor contraction by allowing the number of contracting diagonals to be other than 2, and allowing another binary other than addition
--
-- >>> let b = fromFlatList [2,3] [1..6] :: DArray Int
-- >>> contract sum [1,2] (expand (*) b (transpose b))
-- [[14, 32],
--  [32, 77]]
contract ::
  (DArray a -> b) ->
  [Int] ->
  DArray a ->
  DArray b
contract f xs a = f . diag <$> extractsExcept xs a

-- | a generalisation of a dot operation, which is a multiplicative expansion of two arrays and sum contraction along the middle two dimensions.
--
-- dot sum (*) on two matrices is known as matrix multiplication
--
-- >>> let b = fromFlatList [2,3] [1..6] :: DArray Int
-- >>> dot sum (*) b (transpose b)
-- [[14, 32],
--  [32, 77]]
--
-- dot sum (*) on two vectors is known as the inner product
--
-- >>> let v = fromFlatList [3] [1..3] :: DArray Int
-- >>> dot sum (*) v v
-- 14
--
-- dot sum (*) m v on a matrix and a vector is matrix-vector multiplication
-- Note that an `DArray Int` with shape [3] is neither a row vector nor column vector. `dot` is not turning the vector into a matrix and then using matrix multiplication.
--
-- >>> dot sum (*) v b
-- [9, 12, 15]
--
-- >>> dot sum (*) b v
-- [14, 32]
dot ::
  (DArray c -> d) ->
  (a -> b -> c) ->
  DArray a ->
  DArray b ->
  DArray d
dot f g a b = contract f ([rank sa - 1, rank sa]) (expand g a b)

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
  DArray a ->
  DArray a
slice pss a = tabulate go
  where
    go s = index a (zipWith (!!) pss' s)
    pss' = natValss pss

-- | remove singleton dimensions
--
-- >>> let a' = fromFlatList [2,1,3,4,1] [1..24] :: DArray Int
-- >>> squeeze a
-- [[[1, 2, 3, 4],
--   [5, 6, 7, 8],
--   [9, 10, 11, 12]],
--  [[13, 14, 15, 16],
--   [17, 18, 19, 20],
--   [21, 22, 23, 24]]]
squeeze ::
  DArray a ->
  DArray a
squeeze (DArray s x) = DArray (squeeze s) x

-- * scalar specializations

-- | <https://en.wikipedia.org/wiki/Scalarr_(mathematics) Wiki Scalar>
--
-- An /DArray/ with shape [] despite being a Scalar is nevertheless a one-element vector under the hood.

-- | unwrapping scalars is probably a performance bottleneck
--
-- >>> let s = fromFlatList [] [3] :: DArray Int
-- >>> fromScalar s
-- 3
fromScalar :: DArray a -> a
fromScalar a = index a ([] :: [Int])

-- | convert a number to a scalar
--
-- >>> :t toScalar 2
-- toScalar 2 :: DArray a
toScalar :: a -> DArray a
toScalar a = fromFlatList [] [a]

-- * matrix specializations
-- | extract specialised to a matrix
--
-- >>> row 1 m
-- [4, 5, 6, 7]
row :: forall m n a. (KnownNat m, KnownNat n, HasShape '[m, n]) => Int -> DArray a -> DArray a
row i (DArray s a) = DArray $ V.slice (i * n) n a
  where
    [_,n] = s

-- | extract specialised to a matrix
--
-- >>> col 1 m
-- [1, 5, 9]
col :: forall m n a. (KnownNat m, KnownNat n, HasShape '[m, n]) => Int -> DArray a -> DArray a
col i (DArray s a) = DArray $ V.generate m (\x -> V.unsafeIndex a (i + x * n))
  where
    [m,n] = s

-- | matrix multiplication
--
-- This is dot sum (*) specialised to matrices
--
-- >>> let a = [1, 2, 3, 4] :: DArray '[2, 2] Int
-- >>> let b = [5, 6, 7, 8] :: DArray '[2, 2] Int
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
  DArray a ->
  DArray a ->
  DArray a
mmult (DArray sx x) (DArray sy y) = tabulate [m,n] go
  where
    go [] = throw (NumHaskException "Needs two dimensions")
    go [_] = throw (NumHaskException "Needs two dimensions")
    go (i : j : _) = sum $ V.zipWith (*) (V.slice (fromIntegral i * k) k x) (V.generate k (\x' -> y V.! (fromIntegral j + x' * n)))
    [m,_] = sx
    [_,n] = sy
{-# INLINE mmult #-}


