{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Arrays with a dynamic shape (shape only known at runtime).
module NumHask.Array.Dynamic
  ( -- $usage
    Array (..),
    pattern (:|),
    rank,

    -- * Conversion
    fromFlatList,
    toFlatList,
    fromList1,
    fromList2,

    -- * representable replacements
    index,
    tabulate,

    -- * Operators
    null,
    reshape',
    indices,
    ident,
    sequent,
    diag,
    undiag,
    single,
    singleton,
    selects,
    selectsExcept,
    folds,
    extracts,
    extractsExcept,
    joins,
    maps,
    concatenate,
    insert,
    append,
    reorder,
    expand,
    expandr,
    apply,
    contract,
    dot,
    mult,
    slice,
    squeeze,

    -- * Scalar

    --
    -- Scalar specialisations
    fromScalar,
    toScalar,
    isScalar,

    -- * Matrix

    --
    -- Matrix specialisations.
    col,
    row,
    mmult,
    reverses,
    rotates,
    liftR2,
    takes',
    takes,
    drops,
    reshapeFill,
    liftR2_,
    transpose,
    order,
    orderBy
  )
where

import Data.List qualified as List
import Data.Vector qualified as V
import NumHask.Array.Shape hiding (rank, size)
import NumHask.Prelude as P hiding (null, length)
import NumHask.Prelude qualified as P
import NumHask.Array.Sort
import NumHask.Array.Shape qualified as S
import Prettyprinter hiding (dot)

-- $setup
-- >>> :m -Prelude
-- >>> :set -XDataKinds
-- >>> :set -XOverloadedLists
-- >>> :set -XTypeFamilies
-- >>> :set -XFlexibleContexts
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude
-- >>> import NumHask.Array.Dynamic
-- >>> import Prettyprinter hiding (dot)
-- >>> import NumHask.Array.Shape
-- >>> let s = fromFlatList [] [1] :: Array Int
-- >>> let a = fromFlatList [2,3,4] [1..24] :: Array Int
-- >>> let v = fromFlatList [3] [1,2,3] :: Array Int
-- >>> let m = fromFlatList [3,4] [0..11] :: Array Int

-- $usage
-- >>> :set -XDataKinds
-- >>> :set -XOverloadedLists
-- >>> :set -XTypeFamilies
-- >>> :set -XFlexibleContexts
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude
-- >>> import NumHask.Array.Dynamic
-- >>> import NumHask.Array.Shape
-- >>> import Prettyprinter hiding (dot)
-- >>> let s = fromFlatList [] [1] :: Array Int
-- >>> let a = fromFlatList [2,3,4] [1..24] :: Array Int
-- >>> let v = fromFlatList [3] [1,2,3] :: Array Int
-- >>> let m = fromFlatList [3,4] [0..11] :: Array Int

-- | a multidimensional array with a value-level shape
--
-- >>> let a = fromFlatList [2,3,4] [1..24] :: Array Int
-- >>> a
-- Array {shape = [2,3,4], unArray = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24]}
--
-- pretty a
-- [[[1,2,3,4],
--   [5,6,7,8],
--   [9,10,11,12]],
--  [[13,14,15,16],
--   [17,18,19,20],
--   [21,22,23,24]]]
data Array a = Array {shape :: [Int], unArray :: V.Vector a}
  deriving stock (Generic)
  deriving stock (Eq, Ord, Show)

instance Functor Array where
  fmap f (Array s a) = Array s (V.map f a)

instance Foldable Array where
  foldr x a (Array _ v) = V.foldr x a v

instance Traversable Array where
  traverse f (Array s v) =
    fromFlatList s <$> traverse f (toList v)

instance (Show a) => Pretty (Array a) where
  pretty a = case rank a of
    0 -> viaShow (V.head $ unArray a)
    1 -> viaShow (V.toList $ unArray a)
    _ -> pretty "[" <> indent 0 (vsep (punctuate comma $ pretty <$> (snd $ toFlatList $ extracts [0] a))) <> pretty "]"

rank :: Array a -> Int
rank = S.rank . shape

length :: Array a -> Int
length = S.size . shape

null :: Array a -> Bool
null = (zero==) . length

cons :: Array a -> Array a -> Array a
cons = concatenate 0

uncons :: Array a -> (Array a, Array a)
uncons a = (selects [0] [0] a, selectsExcept [0] [0] a)

infix 5 :|

pattern (:|) :: Array a -> Array a -> Array a
pattern x :| xs <- (uncons -> (x, xs))
  where
    x :| xs = cons x xs

-- * conversions

-- | convert from a (1-D) list, supplying the shape
--
-- >>> fromFlatList [2,3,4] [1..24] == a
-- True
fromFlatList :: [Int] -> [a] -> Array a
fromFlatList ds l = Array ds $ V.fromList $ List.take (S.size ds) l

-- | convert to a (shape, (1-D) list) tuple.
--
-- >>> toFlatList a == ([2,3,4],[1..24])
-- True
--
toFlatList :: Array a -> ([Int], [a])
toFlatList (Array s v) = (s, V.toList v)

-- | Create a rank-1 array
fromList1 :: [a] -> Array a
fromList1 xs = fromFlatList [P.length xs] xs

-- | Create a rank-2 array with the supplied number of rows.
fromList2 :: Int -> [a] -> Array a
fromList2 r xs = fromFlatList [r, P.length xs `div` r] xs

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
tabulate ds f = Array ds . V.generate (S.size ds) $ (f . shapen ds)

liftR2 :: (a -> b -> c) -> Array a -> Array b -> Either String (Array c)
liftR2 f x x' = bool (Left "shape mismatch") (Right $ Array (shape x) (V.zipWith f (unArray x) (unArray x'))) (shape x == shape x')

liftR2_ :: (a -> b -> c) -> Array a -> Array b -> Array c
liftR2_ f x x' = either error id (liftR2 f x x')

-- | Takes the top-most elements according to the new dimension.
--
-- >>> pretty $ takes [2,2,3] a
-- [[[1,2,3],
--   [5,6,7]],
--  [[13,14,15],
--   [17,18,19]]]
takes' ::
  [Int] ->
  Array a ->
  Array a
takes' ds a = tabulate ds $ \s -> index a s

-- | Takes the top-most elements according to the new dimension. Negative values take the bottom-most.
--
-- >>> pretty $ takes [2,2,(-3)] a
-- [[[2,3,4],
--   [6,7,8]],
--  [[14,15,16],
--   [18,19,20]]]
takes ::
  [Int] ->
  Array a ->
  Array a
takes ds a = tabulate ds' $ \s -> index a (zipWith3 (\d' s' a' -> bool s' (s' + a' + d') (d'<0)) ds s (shape a))
  where ds' = fmap abs ds

-- | Drops the top-most elements. Negative values drop the bottom-most.
--
-- >>> pretty $ drops [1,2,(-3)] a
-- [[[21]]]
drops ::
  [Int] ->
  Array a ->
  Array a
drops ds a = tabulate dsNew $ \s -> index a (zipWith (\d' s' -> bool (d' + s') s' (d'<0)) ds s)
  where
    ds' = fmap abs ds
    dsNew = zipWith (\x d -> max 0 (x - d)) (shape a) ds'

-- | reshape an array, supplying a default value for elements outside the dimensions of the old array.
--
reshapeFill ::
  [Int] ->
  a ->
  Array a ->
  Array a
reshapeFill s def a = tabulate s (\xs -> bool def (index a xs) (inside xs (shape a)) )
  where
    inside x y = all id (zipWith (<) x y)

-- | Reshape an array (with the same number of elements).
--
-- >>> pretty $ reshape' [4,3,2] a
-- [[[1,2],
--   [3,4],
--   [5,6]],
--  [[7,8],
--   [9,10],
--   [11,12]],
--  [[13,14],
--   [15,16],
--   [17,18]],
--  [[19,20],
--   [21,22],
--   [23,24]]]
reshape' ::
  [Int] ->
  Array a ->
  Array a
reshape' s a = tabulate s (index a . shapen (shape a) . flatten s)

rotates ::
  [(Int,Int)] ->
  Array a ->
  Array a
rotates rs a = tabulate (shape a) (index a . rotateIndex rs (shape a))

-- | Indices of an Array.
--
-- >>> pretty $ indices [3,3]
-- [[[0,0],[0,1],[0,2]],
--  [[1,0],[1,1],[1,2]],
--  [[2,0],[2,1],[2,2]]]
indices :: [Int] -> Array [Int]
indices ds = tabulate ds id

-- | The identity array.
--
-- >>> pretty $ ident [3,2]
-- [[1,0],
--  [0,1],
--  [0,0]]
ident :: (Additive a, Multiplicative a) => [Int] -> Array a
ident ds = tabulate ds (bool zero one . isDiag)
  where
    isDiag [] = True
    isDiag [_] = True
    isDiag [x, y] = x == y
    isDiag (x : y : xs) = x == y && isDiag (y : xs)

-- | An array of sequential Ints
--
-- >>> pretty $ sequent [3]
-- [0,1,2]
--
-- >>> pretty $ sequent [3,3]
-- [[0,0,0],
--  [0,1,0],
--  [0,0,2]]
sequent :: [Int] -> Array Int
sequent ds = tabulate ds go
  where
    go [] = zero
    go [i] = i
    go (i : js) = bool zero i (all (i ==) js)

-- | Extract the diagonal of an array.
--
-- >>> pretty $ diag (ident [3,2])
-- [1,1]
diag ::
  Array a ->
  Array a
diag a = tabulate [NumHask.Array.Shape.minimum (shape a)] go
  where
    go [] = throw (NumHaskException "Rank Underflow")
    go (s' : _) = index a (replicate (rank a) s')

-- | Expand the array to form a diagonal array
--
-- >>> pretty $ undiag 2 (fromFlatList [2] [1,1])
-- [[1,0],
--  [0,1]]
undiag ::
  (Additive a) =>
  Int ->
  Array a ->
  Array a
undiag r a = tabulate (replicate r (head (shape a))) go
  where
    go [] = throw (NumHaskException "Rank Underflow")
    go xs@(x : xs') = bool zero (index a xs) (all (x ==) xs')


-- | Create an array composed of a single value.
--
-- >>> pretty $ single [3,2] one
-- [[1,1],
--  [1,1],
--  [1,1]]
single :: [Int] -> a -> Array a
single ds a = tabulate ds (const a)

-- | Create an array of dimension [1] composed of a single value.
--
-- >>> pretty $ singleton one
-- [1]
singleton :: a -> Array a
singleton a = Array [1] (V.singleton a)

-- | Select an array along dimensions.
--
-- >>> let s = selects [0,1] [1,1] a
-- >>> pretty s
-- [17,18,19,20]
selects ::
  [Int] ->
  [Int] ->
  Array a ->
  Array a
selects ds i a = tabulate (dropIndexes (shape a) ds) go
  where
    go s = index a (addIndexes s ds i)

-- | Select an index /except/ along specified dimensions
--
-- >>> let s = selectsExcept [2] [1,1] a
-- >>> pretty s
-- [17,18,19,20]
selectsExcept ::
  [Int] ->
  [Int] ->
  Array a ->
  Array a
selectsExcept ds i a = selects (exclude (rank a) ds) i a

-- | Fold along specified dimensions.
--
-- >>> pretty $ folds sum [1] a
-- [68,100,132]
folds ::
  (Array a -> b) ->
  [Int] ->
  Array a ->
  Array b
folds f ds a = tabulate (takeIndexes (shape a) ds) go
  where
    go s = f (selects ds s a)

-- | Extracts dimensions to an outer layer.
--
--
-- > a == (fromScalar <$> extracts [0..rank a] a)
--
-- >>> let e = extracts [1,2] a
-- >>> pretty $ shape <$> extracts [0] a
-- [[3,4],[3,4]]
extracts ::
  [Int] ->
  Array a ->
  Array (Array a)
extracts ds a = tabulate (takeIndexes (shape a) ds) go
  where
    go s = selects ds s a

-- | Extracts /except/ dimensions to an outer layer.
--
-- >>> let e = extractsExcept [1,2] a
-- >>> pretty $ shape <$> extracts [0] a
-- [[3,4],[3,4]]
extractsExcept ::
  [Int] ->
  Array a ->
  Array (Array a)
extractsExcept ds a = extracts (exclude (rank a) ds) a

-- | Join inner and outer dimension layers.
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
    si = shape (index a (replicate (rank a) 0))

-- | Maps a function along specified dimensions.
--
-- >>> shape $ maps (transpose) [1] a
-- [4,3,2]
maps ::
  (Array a -> Array b) ->
  [Int] ->
  Array a ->
  Array b
maps f ds a = joins ds (fmap f (extracts ds a))

-- | Concatenate along a dimension.
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

-- | Insert along a dimension at a position.
--
-- >>> pretty $ insert 2 0 a (fromFlatList [2,3] [100..105])
-- [[[100,1,2,3,4],
--   [101,5,6,7,8],
--   [102,9,10,11,12]],
--  [[103,13,14,15,16],
--   [104,17,18,19,20],
--   [105,21,22,23,24]]]
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

-- | Insert along a dimension at the end.
--
-- >>> pretty $ append 2 a (fromFlatList [2,3] [100..105])
-- [[[1,2,3,4,100],
--   [5,6,7,8,101],
--   [9,10,11,12,102]],
--  [[13,14,15,16,103],
--   [17,18,19,20,104],
--   [21,22,23,24,105]]]
append ::
  Int ->
  Array a ->
  Array a ->
  Array a
append d a b = insert d (dimension (shape a) d) a b

-- | change the order of dimensions
--
-- >>> let r = reorder [2,0,1] a
-- >>> pretty r
-- [[[1,5,9],
--   [13,17,21]],
--  [[2,6,10],
--   [14,18,22]],
--  [[3,7,11],
--   [15,19,23]],
--  [[4,8,12],
--   [16,20,24]]]
reorder ::
  [Int] ->
  Array a ->
  Array a
reorder ds a = tabulate (reorder' (shape a) ds) go
  where
    go s = index a (addIndexes [] ds s)

-- | reverses order along specified dimensions.
--
-- > reverses [0] a
--
reverses ::
  [Int] ->
  Array a ->
  Array a
reverses ds a = tabulate (shape a) (index a . reverseIndex ds (shape a))

-- | Product two arrays using the supplied binary function.
--
-- For context, if the function is multiply, and the arrays are tensors,
-- then this can be interpreted as a tensor product.
--
-- https://en.wikipedia.org/wiki/Tensor_product
--
-- The concept of a tensor product is a dense crossroad, and a complete treatment is elsewhere.  To quote:
-- ... the tensor product can be extended to other categories of mathematical objects in addition to vector spaces, such as to matrices, tensors, algebras, topological vector spaces, and modules. In each such case the tensor product is characterized by a similar universal property: it is the freest bilinear operation. The general concept of a "tensor product" is captured by monoidal categories; that is, the class of all things that have a tensor product is a monoidal category.
--
-- >>> pretty $ expand (*) v v
-- [[1,2,3],
--  [2,4,6],
--  [3,6,9]]
--
-- Alternatively, expand can be understood as representing the permutation of element pairs of two arrays, so like the Applicative List instance.
--
-- >>> i2 = indices [2,2]
-- >>> pretty $ expand (,) i2 i2
-- [[[[([0,0],[0,0]),([0,0],[0,1])],
--    [([0,0],[1,0]),([0,0],[1,1])]],
--   [[([0,1],[0,0]),([0,1],[0,1])],
--    [([0,1],[1,0]),([0,1],[1,1])]]],
--  [[[([1,0],[0,0]),([1,0],[0,1])],
--    [([1,0],[1,0]),([1,0],[1,1])]],
--   [[([1,1],[0,0]),([1,1],[0,1])],
--    [([1,1],[1,0]),([1,1],[1,1])]]]]
expand ::
  (a -> b -> c) ->
  Array a ->
  Array b ->
  Array c
expand f a b = tabulate ((++) (shape a) (shape b)) (\i -> f (index a (List.take r i)) (index b (List.drop r i)))
  where
    r = rank a

-- | Like expand, but permutes the first array first, rather than the second.
--
-- >>> pretty $ expand (,) v (fmap (+3) v)
-- [[(1,4),(1,5),(1,6)],
--  [(2,4),(2,5),(2,6)],
--  [(3,4),(3,5),(3,6)]]
--
-- >>> pretty $ expandr (,) v (fmap (+3) v)
-- [[(1,4),(2,4),(3,4)],
--  [(1,5),(2,5),(3,5)],
--  [(1,6),(2,6),(3,6)]]
expandr ::
  (a -> b -> c) ->
  Array a ->
  Array b ->
  Array c
expandr f a b = tabulate ((++) (shape a) (shape b)) (\i -> f (index a (List.drop r i)) (index b (List.take r i)))
  where
    r = rank a

-- | Apply an array of functions to each array of values.
--
-- This is in the spirit of the applicative functor operation (\<*\>).
--
-- > expand f a b == apply (fmap f a) b
--
-- >>> pretty $ apply ((*) <$> v) v
-- [[1,2,3],
--  [2,4,6],
--  [3,6,9]]
--
-- Dynamic arrays can't be Applicatives because there is no 'pure' (Shape is not known at compile-time).
--
-- >>> let b = fromFlatList [2,3] [1..6] :: Array Int
-- >>> pretty $ contract sum [1,2] (apply (fmap (*) b) (transpose b))
-- [[14,32],
--  [32,77]]
apply ::
  Array (a -> b) ->
  Array a ->
  Array b
apply f a = tabulate ((++) (shape f) (shape a)) (\i -> index f (List.take r i) (index a (List.drop r i)))
  where
    r = rank a

-- | Contract an array by applying the supplied (folding) function on diagonal elements of the dimensions.
--
-- This generalises a tensor contraction by allowing the number of contracting diagonals to be other than 2, and allowing a binary operator other than multiplication.
--
-- >>> let b = fromFlatList [2,3] [1..6] :: Array Int
-- >>> pretty $ contract sum [1,2] (expand (*) b (transpose b))
-- [[14,32],
--  [32,77]]
contract ::
  (Array a -> b) ->
  [Int] ->
  Array a ->
  Array b
contract f xs a = f . diag <$> extractsExcept xs a

-- | A generalisation of a dot operation, which is a multiplicative expansion of two arrays and sum contraction along the middle two dimensions.
--
-- matrix multiplication
--
-- >>> let b = fromFlatList [2,3] [1..6] :: Array Int
-- >>> pretty $ dot sum (*) b (transpose b)
-- [[14,32],
--  [32,77]]
--
-- inner product
--
-- >>> let v = fromFlatList [3] [1..3] :: Array Int
-- >>> pretty $ dot sum (*) v v
-- 14
--
-- matrix-vector multiplication
-- Note that an `Array Int` with shape [3] is neither a row vector nor column vector. `dot` is not turning the vector into a matrix and then using matrix multiplication.
--
-- >>> pretty $ dot sum (*) v b
-- [9,12,15]
--
-- >>> pretty $ dot sum (*) b v
-- [14,32]
dot ::
  (Array c -> d) ->
  (a -> b -> c) ->
  Array a ->
  Array b ->
  Array d
dot f g a b = contract f [r - 1, r] (expand g a b)
  where
    r = rank a

-- | Array multiplication.
--
-- matrix multiplication
--
-- >>> let b = fromFlatList [2,3] [1..6] :: Array Int
-- >>> pretty $ mult b (transpose b)
-- [[14,32],
--  [32,77]]
--
-- inner product
--
-- >>> let v = fromFlatList [3] [1..3] :: Array Int
-- >>> pretty $ mult v v
-- 14
--
-- matrix-vector multiplication
--
-- >>> pretty $ mult v b
-- [9,12,15]
--
-- >>> pretty $ mult b v
-- [14,32]
mult ::
  ( Additive a,
    Multiplicative a
  ) =>
  Array a ->
  Array a ->
  Array a
mult = dot sum (*)

-- | Select elements along positions in every dimension.
--
-- >>> let s = slice [[0,1],[0,2],[1,2]] a
-- >>> pretty s
-- [[[2,3],
--   [10,11]],
--  [[14,15],
--   [22,23]]]
slice ::
  [[Int]] ->
  Array a ->
  Array a
slice pss a = tabulate (ranks pss) go
  where
    go s = index a (zipWith (!!) pss s)

-- | Remove single dimensions.
--
-- >>> let a' = fromFlatList [2,1,3,4,1] [1..24] :: Array Int
-- >>> shape $ squeeze a'
-- [2,3,4]
squeeze ::
  Array a ->
  Array a
squeeze (Array s x) = Array (squeeze' s) x

-- | Unwrapping scalars is probably a performance bottleneck.
--
-- >>> let s = fromFlatList [] [3] :: Array Int
-- >>> fromScalar s
-- 3
fromScalar :: Array a -> a
fromScalar a = index a ([] :: [Int])

-- | Convert a number to a scalar.
--
-- >>> :t toScalar 2
-- toScalar 2 :: FromInteger a => Array a
toScalar :: a -> Array a
toScalar a = fromFlatList [] [a]

-- | Is the Array a Scalar
--
-- >>> isScalar (toScalar (2::Int))
-- True
isScalar :: Array a -> Bool
isScalar a = List.null (shape a)

-- | Extract specialised to a matrix.
--
-- >>> pretty $ row 1 m
-- [4,5,6,7]
row :: Int -> Array a -> Array a
row i (Array s a) = Array [n] $ V.slice (i * n) n a
  where
    (_ : n : _) = s

-- | extract specialised to a matrix
--
-- >>> pretty $ col 1 m
-- [1,5,9]
col :: Int -> Array a -> Array a
col i (Array s a) = Array [m] $ V.generate m (\x -> V.unsafeIndex a (i + x * n))
  where
    (m : n : _) = s

-- | matrix multiplication
--
-- This is dot sum (*) specialised to matrices
--
-- >>> let a = fromFlatList [2,2] [1,2,3,4] :: Array Int
-- >>> let b = fromFlatList [2,2] [5,6,7,8] :: Array Int
-- >>> pretty a
-- [[1,2],
--  [3,4]]
--
-- >>> pretty b
-- [[5,6],
--  [7,8]]
--
-- >>> pretty $ mmult a b
-- [[19,22],
--  [43,50]]
mmult ::
  (Ring a) =>
  Array a ->
  Array a ->
  Array a
mmult (Array sx x) (Array sy y) = tabulate [m, n] go
  where
    go [] = throw (NumHaskException "Needs two dimensions")
    go [_] = throw (NumHaskException "Needs two dimensions")
    go (i : j : _) = sum $ V.zipWith (*) (V.slice (fromIntegral i * k) k x) (V.generate k (\x' -> y V.! (fromIntegral j + x' * n)))
    (m : k : _) = sx
    (_ : n : _) = sy
{-# INLINE mmult #-}

-- |
-- >>> pretty $ transpose (fromFlatList [2,2,2] [1..8])
-- [[[1,5],
--   [3,7]],
--  [[2,6],
--   [4,8]]]
--
-- FIXME: huihua example transposes 001 to 010. A 1 rotation.
-- This transposes 001 to 100
-- | Reverse indices eg transposes the element A/ijk/ to A/kji/.
--
-- >>> index (transpose a) [1,0,0] == index a [0,0,1]
-- True
transpose :: Array a -> Array a
transpose a = tabulate (List.reverse $ shape a) (index a . List.reverse)

order :: (Ord a) => Array a -> Array Int
order (Array s v) = Array s (orderV v)

orderBy :: (Ord b) => (a -> b) -> Array a -> Array Int
orderBy c (Array s v) = Array s (orderByV c v)
