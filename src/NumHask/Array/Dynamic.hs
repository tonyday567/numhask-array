{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | Arrays with a dynamic shape (shape only known at runtime).
module NumHask.Array.Dynamic
  ( -- $usage
    Array (..),
    FromVector (..),
    FromArray (..),
    array,
    validate,
    safeArray,

    -- * Shape interogation
    shape,
    rank,
    size,
    null,

    -- * indexing
    index,
    tabulate,
    tabulateA,
    flatten,
    shapen,
    shapenA,

    -- Scalar conversions
    fromScalar,
    toScalar,
    isScalar,

    -- * Poly-Dimensional Operators
    indices,
    ident,
    diag,
    undiag,
    konst,
    singleton,
    takes,
    drops,
    selects,
    folds,
    extracts,
    extractsExcept,
    joins,
    maps,
    concatenate,
    insert,
    append,
    expand,
    expandr,
    contract,
    dot,
    mult,
    slice,
    slice',

    -- * shape manipulations
    reorder,
    reshape,
    reshapeFill,
    squeeze,
    stretch,
    reverses,
    rotates,
    transpose,

    order,
    orderBy,

    -- * row specializations
    pattern (:|),
    row,
    take,
    drop,
    zip,

    -- * column specializations
    col,

    -- * element-level specializations
    zipE,
    zipWithE,
    safeZipE,
    safeZipWithE,

    -- * array specializations
    range,
    iota,
    join,
  )
where

import Data.List qualified as List
import Data.Vector qualified as V
import NumHask.Prelude as P hiding (take, drop, zip, null, length, Scalar)
import NumHask.Array.Sort
import NumHask.Array.Shape qualified as S
import Prettyprinter hiding (dot)

-- $setup
-- >>> :m -Prelude
-- >>> :set -XDataKinds
-- >>> :set -XTypeFamilies
-- >>> :set -XFlexibleContexts
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude
-- >>> import NumHask.Array.Dynamic
-- >>> import Prettyprinter hiding (dot)
-- >>> import NumHask.Array.Shape qualified as S
-- >>> let s = array [] [1] :: Array Int
-- >>> let a = array [2,3,4] [1..24] :: Array Int
-- >>> let v = array [3] [1,2,3] :: Array Int
-- >>> let m = array [3,4] [0..11] :: Array Int

-- $usage
-- >>> :set -XDataKinds
-- >>> :set -XTypeFamilies
-- >>> :set -XFlexibleContexts
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude
-- >>> import NumHask.Array.Dynamic
-- >>> import NumHask.Array.Shape
-- >>> import Prettyprinter hiding (dot)
-- >>> let s = array [] [1] :: Array Int
-- >>> let a = array [2,3,4] [1..24] :: Array Int
-- >>> let v = array [3] [1,2,3] :: Array Int
-- >>> let m = array [3,4] [0..11] :: Array Int

-- | a multidimensional array with a value-level shape
--
-- >>> let a = array [2,3,4] [1..24] :: Array Int
-- >>> a
-- UnsafeArray [2,3,4] [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24]
--
-- pretty a
-- [[[1,2,3,4],
--   [5,6,7,8],
--   [9,10,11,12]],
--  [[13,14,15,16],
--   [17,18,19,20],
--   [21,22,23,24]]]
data Array a = UnsafeArray (V.Vector Int) (V.Vector a)
  deriving stock (Generic)
  deriving stock (Eq, Ord, Show)

instance Functor Array where
  fmap f (UnsafeArray s a) = UnsafeArray s (V.map f a)

instance Foldable Array where
  foldr x a (UnsafeArray _ v) = V.foldr x a v

instance Traversable Array where
  traverse f (UnsafeArray s v) =
    array s <$> traverse f v

instance (Show a) => Pretty (Array a) where
  pretty a@(UnsafeArray _ v) = case rank a of
    0 -> viaShow (V.head v)
    1 -> viaShow v
    _ ->
      pretty "[" <>
      indent 0 (vsep (punctuate comma $ pretty <$>
        (toList $ extracts [0] a))) <>
      pretty "]"

-- * conversions
instance (FromInteger a) => FromInteger (Array a) where
  fromInteger x = UnsafeArray V.empty (V.singleton (fromInteger x))

instance (FromRational a) => FromRational (Array a) where
  fromRational x = UnsafeArray V.empty (V.singleton (fromRational x))

class FromVector t a | t -> a where
  asVector :: t -> V.Vector a
  vectorAs :: V.Vector a -> t

instance FromVector (V.Vector a) a where
  asVector = id
  vectorAs = id

instance FromVector [a] a where
  asVector = V.fromList
  vectorAs = V.toList

instance FromVector (Array a) a where
  asVector (UnsafeArray _ v) = v
  vectorAs v = UnsafeArray (V.singleton (V.length v)) v

class FromArray t a | t -> a where
  asArray :: t -> Array a
  arrayAs :: Array a -> t

instance FromArray (Array a) a where
  asArray = id
  arrayAs = id

instance FromArray [a] a where
  asArray l = UnsafeArray (V.singleton (S.rank l)) (V.fromList l)
  arrayAs (UnsafeArray _ v) = V.toList v

instance FromArray (V.Vector a) a where
  asArray v = UnsafeArray (V.singleton (V.length v)) v
  arrayAs (UnsafeArray _ v) = v

-- | Construct an array from shape and value vectors without any shape validation
--
-- >>> array [2,3,4] [1..24] == a
-- True
array :: FromVector u Int => FromVector t a => u -> t -> Array a
array (asVector -> s) (asVector -> v) = UnsafeArray s v

validate :: Array a -> Bool
validate a = V.product (shape a) == V.length (asVector a)

safeArray :: FromVector u Int => FromVector t a => u -> t -> Maybe (Array a)
safeArray s v =
  bool Nothing (Just a) (validate a)
  where
    a = UnsafeArray (asVector s) (asVector v)

shape :: FromVector u Int => Array a -> u
shape (UnsafeArray s _) = vectorAs s

rank :: Array a -> Int
rank = V.length . shape

size :: Array a -> Int
size = V.product . shape

null :: Array a -> Bool
null = (zero==) . size

flattenV :: V.Vector Int -> V.Vector Int -> Int
flattenV ns xs = V.sum $ V.zipWith (*) xs (V.drop 1 $ V.scanr (*) one ns)

shapenV :: V.Vector Int -> Int -> V.Vector Int
shapenV ns x = V.fromList $ S.shapen (V.toList ns) x

flatten :: (FromVector u Int) => u -> u -> Int
flatten ns xs = flattenV (asVector ns) (asVector xs)

shapen :: (FromVector u Int) => u -> Int -> u
shapen ns x = vectorAs $ shapenV (asVector ns) x

-- | shapenA preserves the shape information of the shaped index. Practically this is needed to distinguish between a scalar and a 1-element array.
--
-- >>> shapenA (toScalar 3) 2
-- UnsafeArray [] [2]
-- >>> shapenA (array [1] [3]) 2
-- UnsafeArray [1] [2]
shapenA :: (FromArray u Int) => u -> Int -> u
shapenA ns x = arrayAs $ UnsafeArray (shape $ asArray ns) (shapenV (arrayAs $ asArray ns) x)

-- | extract an element at index /i/
--
-- >>> index a [1,2,3]
-- 24
index :: (FromVector u Int) => Array a -> u -> a
index (UnsafeArray s v) i = V.unsafeIndex v (flatten s (asVector i))

-- | tabulate an array with a generating function
--
-- >>> tabulate [2,3,4] ((1+) . flatten [2,3,4]) == a
-- True
tabulate :: (FromVector u Int) => u -> (u -> a) -> Array a
tabulate ds f =
  UnsafeArray (asVector ds) (V.generate (V.product (asVector ds)) (\i -> f (shapen ds i)))

-- | tabulate an array with a generating function, with the shape supplied as an Array
--
tabulateA :: (FromArray u Int, FromVector u Int) => u -> (u -> a) -> Array a
tabulateA ds f =
  UnsafeArray (asVector ds) (V.generate (V.product (asVector ds)) (\i -> f (shapenA ds i)))


-- | Unwrapping scalars is probably a performance bottleneck.
--
-- >>> let s = array [] [3] :: Array Int
-- >>> fromScalar s
-- 3
fromScalar :: Array a -> a
fromScalar a = index a ([] :: [Int])

-- | Convert a number to a scalar.
--
-- >>> :t toScalar 2
-- toScalar 2 :: FromInteger a => Array a
toScalar :: a -> Array a
toScalar a = tabulate [] (const a)

-- | Is the Array a Scalar
--
-- >>> isScalar (toScalar (2::Int))
-- True
isScalar :: Array a -> Bool
isScalar a = rank a == zero

-- * operations

-- | Indices of an Array.
--
-- >>> pretty $ indices [3,3]
-- [[[0,0],[0,1],[0,2]],
--  [[1,0],[1,1],[1,2]],
--  [[2,0],[2,1],[2,2]]]
indices :: (FromVector u Int) => u -> Array u
indices ds = tabulate ds id

-- | Takes the top-most elements across the supplied dimensions. Negative values take the bottom-most.
--
-- >>> pretty $ takes [(0,1), (1,2), (2,-3)] a
-- [[[2,3,4],
--   [6,7,8]]]
takes ::
  (FromVector u (Int,Int)) =>
  u ->
  Array a ->
  Array a
takes ts a = tabulate dsNew $ \s -> index a (V.zipWith3 (\d' s' a' -> bool s' (s' + a' + d') (d'<0)) (asVector xsNew) (asVector s) (shape a))
  where
    dsNew = S.replaceIndexes ds xsAbs (shape a)
    xsNew = S.replaceIndexes ds xs (replicate (rank a) 0)
    ds = vectorAs $ V.map fst (asVector ts)
    xs = vectorAs $ V.map snd (asVector ts)
    xsAbs = vectorAs (V.map abs (asVector xs))

-- | Drops the top-most elements. Negative values drop the bottom-most.
--
-- >>> pretty $ drops [(0,1), (1,2), (2,-3)] a
-- [[[21]]]
drops ::
  (FromVector u (Int,Int)) =>
  u ->
  Array a ->
  Array a
drops ts a = tabulate dsNew $ \s -> index a (V.zipWith (\d' s' -> bool (d' + s') s' (d'<0)) (asVector xsNew) (asVector s))
  where
    dsNew = S.modifyIndexes ds (fmap (flip (-)) xsAbs) (shape a)
    xsNew = S.replaceIndexes ds xs (replicate (rank a) 0)
    ds = vectorAs $ V.map fst (asVector ts)
    xs = vectorAs $ V.map snd (asVector ts)
    xsAbs = vectorAs (V.map abs (asVector xs))

-- | reshape an array, supplying a default value for elements outside the dimensions of the old array.
--
reshapeFill ::
  (FromVector u Int) =>
  u ->
  a ->
  Array a ->
  Array a
reshapeFill s def a = tabulate s (\xs -> bool def (index a xs) (inside (asVector xs) (shape a)) )
  where
    inside x y = all id (V.zipWith (<) x y)

-- | Reshape an array (with the same number of elements).
--
-- >>> pretty $ reshape [4,3,2] a
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
reshape ::
  (FromVector u Int) =>
  u ->
  Array a ->
  Array a
reshape s a = tabulate s (index @(V.Vector Int) a . shapen (shape a) . flatten s)

rotates ::
  [(Int,Int)] ->
  Array a ->
  Array a
rotates rs a = tabulate (shape a) (index a . S.rotateIndex rs (shape a))

-- | The identity array.
--
-- >>> pretty $ ident [3,2]
-- [[1,0],
--  [0,1],
--  [0,0]]
ident :: (Additive a, Multiplicative a, FromVector u Int) => u -> Array a
ident ds = tabulate ds (bool zero one . S.isDiag . vectorAs . asVector)


-- | Extract the diagonal of an array.
--
-- >>> pretty $ diag (ident [3,2])
-- [1,1]
diag ::
  Array a ->
  Array a
diag a = tabulate (minRank (shape a)) (\xs -> index a (replicate (rank a) (head xs)))
  where
    minRank [] = []
    minRank xs = [S.minimum xs]

-- | Expand the array to form a diagonal array
--
-- >>> pretty $ undiag 2 (array [2] [1,1])
-- [[1,0],
--  [0,1]]
undiag ::
  (Additive a) =>
  Int ->
  Array a ->
  Array a
undiag r a = tabulate (replicate r (head (shape a))) (\xs -> bool zero (index a xs) (S.isDiag xs))

-- | Create an array composed of a single value.
--
-- >>> pretty $ konst [3,2] one
-- [[1,1],
--  [1,1],
--  [1,1]]
konst :: (FromVector u Int) => u -> a -> Array a
konst ds a = tabulate ds (const a)

-- | Create an array of rank 1, shape [1].
--
-- >>> pretty $ singleton one
-- [1]
-- >>> singleton 3 == toScalar 3
-- False
--
-- >>> asVector (singleton 3) == asVector (toScalar 3)
-- True
singleton :: a -> Array a
singleton a = UnsafeArray (V.singleton 1) (V.singleton a)

-- | Select (dimension,index) pairs.
--
-- >>> let s = selects [(0,1),(1,1)] a
-- >>> pretty s
-- [17,18,19,20]
selects ::
  (Foldable u, Functor u) =>
  u (Int, Int) ->
  Array a ->
  Array a
selects ds a = tabulate (S.dropIndexes (shape a) ds') go
  where
    go s = index a (S.addIndexes s ds' xs)
    ds' = toList (fmap fst ds)
    xs = toList (fmap snd ds)

-- | Fold along specified dimensions.
--
-- >>> pretty $ folds [1] sum a
-- [68,100,132]
folds ::
  [Int] ->
  (Array a -> b) ->
  Array a ->
  Array b
folds ds f a = tabulate (S.takeIndexes (shape a) ds) go
  where
    go s = f (selects (List.zip ds s) a)

-- | Extracts dimensions to an outer layer.
--
--
-- > a == (fromScalar <$> extracts [0..rank a] a)
--
-- >>> pretty $ (shape @[Int]) <$> extracts [0] a
-- [[3,4],[3,4]]
extracts ::
  [Int] ->
  Array a ->
  Array (Array a)
extracts ds a = tabulate (S.takeIndexes (shape a) ds) go
  where
    go s = selects (List.zip ds s) a

-- | Extracts /except/ dimensions to an outer layer.
--
-- >>> let e = extractsExcept [1,2] a
-- >>> pretty $ shape @[Int] <$> extracts [0] a
-- [[3,4],[3,4]]
extractsExcept ::
  [Int] ->
  Array a ->
  Array (Array a)
extractsExcept ds a = extracts (S.exclude (rank a) ds) a

-- | Join inner and outer dimension layers by supplied dimensions.
--
-- >>> let e = extracts [1,0] a
-- >>> let j = joins [1,0] e
-- >>> a == j
-- True
joins ::
  [Int] ->
  Array (Array a) ->
  Array a
joins ds a = tabulate (S.addIndexes si ds so) go
  where
    go s = index (index a (S.takeIndexes s ds)) (S.dropIndexes s ds)
    so = shape a
    si = shape (index a (replicate (rank a) 0))

-- | Maps a function along specified dimensions.
--
-- >>> shape @[Int] $ maps [1] transpose a
-- [4,3,2]
maps ::
  [Int] ->
  (Array a -> Array b) ->
  Array a ->
  Array b
maps ds f a = joins ds (fmap f (extracts ds a))

-- | Concatenate along a dimension.
--
-- >>> shape @[Int] $ concatenate 1 a a
-- [2,6,4]
concatenate ::
  Int ->
  Array a ->
  Array a ->
  Array a
concatenate d a0 a1 = tabulate (S.concatenate' d (shape a0) (shape a1)) go
  where
    go s =
      bool
        (index a0 s)
        ( index
            a1
            ( S.addIndex
                (S.dropIndex s d)
                d
                ((s !! d) - (ds0 !! d))
            )
        )
        ((s !! d) >= (ds0 !! d))
    ds0 = shape a0

-- | Insert along a dimension at a position.
--
-- >>> pretty $ insert 2 0 a (array [2,3] [100..105])
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
insert d i a b = tabulate (S.incAt d (shape a)) go
  where
    go s
      | s !! d == i = index b (S.dropIndex s d)
      | s !! d < i = index a s
      | otherwise = index a (S.decAt d s)

-- | Insert along a dimension at the end.
--
-- >>> pretty $ append 2 a (array [2,3] [100..105])
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
append d a b = insert d (S.dimension (shape a) d) a b

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
reorder ds a = tabulate (S.reorder' (shape a) ds) go
  where
    go s = index a (S.addIndexes [] ds s)

-- | reverses order along specified dimensions.
--
-- >>> pretty $ reverses [0,1] a
-- [[[21,22,23,24],
--   [17,18,19,20],
--   [13,14,15,16]],
--  [[9,10,11,12],
--   [5,6,7,8],
--   [1,2,3,4]]]
reverses ::
  [Int] ->
  Array a ->
  Array a
reverses ds a = tabulate (shape a) (index a . S.reverseIndex ds (shape a))

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
expand f a b = tabulate (shape a <> shape b) (\i -> f (index a (List.take r i)) (index b (List.drop r i)))
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
expandr f a b = tabulate (shape a <> shape b) (\i -> f (index a (List.drop r i)) (index b (List.take r i)))
  where
    r = rank a

-- | Contract an array by applying the supplied (folding) function on diagonal elements of the dimensions.
--
-- This generalises a tensor contraction by allowing the number of contracting diagonals to be other than 2, and allowing a binary operator other than multiplication.
--
-- >>> let b = array [2,3] [1..6] :: Array Int
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
-- >>> let b = array [2,3] [1..6] :: Array Int
-- >>> pretty $ dot sum (*) b (transpose b)
-- [[14,32],
--  [32,77]]
--
-- inner product
--
-- >>> let v = array [3] [1..3] :: Array Int
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
-- >>> let b = array [2,3] [1..6] :: Array Int
-- >>> pretty $ mult b (transpose b)
-- [[14,32],
--  [32,77]]
--
-- inner product
--
-- >>> let v = array [3] [1..3] :: Array Int
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
slice pss a = tabulate (S.ranks pss) go
  where
    go s = index a (zipWith (!!) pss s)

-- | Slice along a dimension.
--
-- >>> let s = slice' 2 1 2 a
-- >>> pretty s
-- [[[2,3],
--   [6,7],
--   [10,11]],
--  [[14,15],
--   [18,19],
--   [22,23]]]
slice' ::
  Int ->
  Int ->
  Int ->
  Array a ->
  Array a
slice' d offset l a = takes [(d,l)] $ drops [(d,offset)] a


-- | Remove single dimensions.
--
-- >>> let a' = array [2,1,3,4,1] [1..24] :: Array Int
-- >>> shape @[Int] $ squeeze a'
-- [2,3,4]
squeeze ::
  Array a ->
  Array a
squeeze (UnsafeArray s x) = UnsafeArray (V.fromList $ S.squeeze' (V.toList s)) x

-- | Insert a single dimension at the supplied position.
--
-- >>> shape @[Int] $ stretch 1 a
-- [2,1,3,4]
stretch ::
  Int ->
  Array a ->
  Array a
stretch d (UnsafeArray s x) = UnsafeArray (V.fromList $ S.addIndex (V.toList s) d 1) x

-- | Reverse indices eg transposes the element A/ijk/ to A/kji/.
-- FIXME: huihua example transposes 001 to 010. A 1 rotation.
-- This transposes 001 to 100
--
-- >>> index (transpose a) [1,0,0] == index a [0,0,1]
-- True
-- >>> pretty $ transpose (array [2,2,2] [1..8])
-- [[[1,5],
--   [3,7]],
--  [[2,6],
--   [4,8]]]
--
transpose :: Array a -> Array a
transpose a = tabulate (List.reverse $ shape a) (index a . List.reverse)

order :: (Ord a) => Array a -> Array Int
order (UnsafeArray s v) = UnsafeArray s (orderV v)

orderBy :: (Ord b) => (a -> b) -> Array a -> Array Int
orderBy c (UnsafeArray s v) = UnsafeArray s (orderByV c v)

-- * row (first dimension) specializations

-- | selects specialised to selecting a single selection across the first dimension.
--
-- >>> pretty $ row 1 m
-- [4,5,6,7]
row :: Int -> Array a -> Array a
row i a = selects [(0,i)] a

cons :: Array a -> Array a -> Array a
cons = concatenate 0

uncons :: Array a -> (Array a, Array a)
uncons a = (selects [(0,0)] a, drops [(0,1)] a)

infix 5 :|

pattern (:|) :: Array a -> Array a -> Array a
pattern x :| xs <- (uncons -> (x, xs))
  where
    x :| xs = cons x xs

{-# complete (:|) :: Array #-}

take :: Int -> Array a -> Array a
take n a = takes [(0,n)] a

drop :: Int -> Array a -> Array a
drop n a = drops [(0,n)] a

zip :: Array a -> Array b -> Array (a,b)
zip (a :| as) (b :| bs) = zipE a b :| zipE as bs

-- * column (last dimension) specizations

-- | selects specialised to selecting a single selection across the last dimension.
--
-- >>> pretty $ col 1 m
-- [1,5,9]
col :: Int -> Array a -> Array a
col i a = selects [(rank a - 1,i)] a

-- * element level specializations
safeZipE :: Array a -> Array b -> Either String (Array (a,b))
safeZipE = safeZipWithE (,)

safeZipWithE :: (a -> b -> c) -> Array a -> Array b -> Either String (Array c)
safeZipWithE f (UnsafeArray s v) (UnsafeArray s' v') = bool (Left "shape mismatch") (Right $ UnsafeArray s (V.zipWith f v v')) (s == s')

zipWithE :: (a -> b -> c) -> Array a -> Array b -> Array c
zipWithE f (UnsafeArray s v) (UnsafeArray _ v') = UnsafeArray s (V.zipWith f v v')

zipE :: Array a -> Array b -> Array (a,b)
zipE (UnsafeArray s v) (UnsafeArray _ v') = UnsafeArray s (V.zip v v')

-- * array specializations

-- | indices specialized to an expanded array
--
-- >>> pretty $ range (toScalar 3)
-- [0,1,2]
--
-- >>> pretty $ range [3]
-- [[0],
--  [1],
--  [2]]
--
-- >>> pretty $ range [3,3]
-- [[[0,0],
--   [0,1],
--   [0,2]],
--  [[1,0],
--   [1,1],
--   [1,2]],
--  [[2,0],
--   [2,1],
--   [2,2]]]
range :: (FromArray u Int) => u -> Array Int
range a = join (tabulateA (asArray a) id)

iota :: Int -> Array Int
iota n = array [n] [0..(n-1)]

-- | Join inner and outer dimension layers in outer dimension order.
--
-- >>> a == join (extracts [0,1] a)
-- True
join ::
  Array (Array a) ->
  Array a
join a = joins [0..rank a - 1] a
