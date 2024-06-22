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
    (><),
    validate,
    safeArray,
    unsafeModifyShape,
    unsafeModifyVector,
    def,
    empty,

    -- * Shape interogation
    shape,
    rank,
    size,
    length,
    isNull,

    -- * indexing
    index,
    tabulate,
    tabulateA,
    flatten,
    shapen,
    shapenA,
    backpermute,
    backpermute2,

    -- Scalar conversions
    fromScalar,
    toScalar,
    isScalar,
    asDimmed,
    asScalar,

    -- * Poly-Dimensional Operators
    indices,
    ident,
    diag,
    undiag,
    konst,
    singleton,
    takes,
    takeD,
    takeDs,
    drops,
    dropD,
    dropDs,
    selectD,
    selects,
    reduces,
    traverses,
    extracts,
    extractsExcept,
    joins,
    joinsSafe,
    maps,
    zips,
    zipsSafe,
    diffs,
    concatenate,
    insert,
    append,
    prepend,
    expand,
    expandr,
    contract,
    dot,
    mult,
    slice,
    sliceD,

    -- * shape manipulations
    reorder,
    reshape,
    reshapeDef,
    squeeze,
    elongate,
    reverses,
    rotates,
    transpose,
    inflate,
    inflates,

    -- * sorting
    sorts,
    sortsBy,
    orders,
    ordersBy,

    -- * transmission
    transmit,
    transmitSafe,
    telecasts,
    telecastsSafe,

    -- * row specializations
    pattern (:|),
    row,
    cons,
    uncons,
    take,
    drop,

    -- * column specializations
    col,

    -- * element-level specializations
    zipWithE,
    zipWithESafe,
    diff,

    -- * array specializations
    range,
    iota,
    join,
    joinSafe,

  )
where

import Data.List qualified as List
import Data.Vector qualified as V
import NumHask.Prelude as P hiding (diff, empty, take, drop, zip, zipWith, length)
import NumHask.Array.Sort
import NumHask.Array.Shape qualified as S
import Prettyprinter hiding (dot)
import Data.Bifunctor

-- $setup
-- >>> :m -Prelude
-- >>> :set -XDataKinds
-- >>> :set -XTypeFamilies
-- >>> :set -XFlexibleContexts
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude hiding (empty, diff)
-- >>> import NumHask.Array.Dynamic as D
-- >>> import Prettyprinter hiding (dot)
-- >>> import NumHask.Array.Shape qualified as S
-- >>> import Data.Vector qualified as V
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
-- >>> import NumHask.Array.Dynamic as D
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

-- | Polymorphic constructor of an array from shape and value vectors without any shape validation
--
-- >>> array [2,3,4] [1..24] == a
-- True
array :: FromVector u Int => FromVector t a => u -> t -> Array a
array (asVector -> s) (asVector -> v) = UnsafeArray s v

infixl 4 ><

-- | Construct an Array.
--
-- >>> pretty $ [2,3] >< [0..5]
-- [[0,1,2],
--  [3,4,5]]
(><) :: FromVector u Int => FromVector t a => u -> t -> Array a
(><) = array

-- | Validate the size and shape of an array.
--
-- >>> validate (array [2,3,4] [1..23] :: Array Int)
-- False
validate :: Array a -> Bool
validate a = V.product (shape a) == V.length (asVector a)

-- | Construct an Array, checking shape.
--
-- >>> safeArray [2,3,4] [1..24] == Just a
-- True
safeArray :: FromVector u Int => FromVector t a => u -> t -> Maybe (Array a)
safeArray s v =
  bool Nothing (Just a) (validate a)
  where
    a = UnsafeArray (asVector s) (asVector v)

-- | Unsafely modify an array shape
--
-- >>> unsafeModifyShape (fmap (+1) :: [Int] -> [Int]) (array [2,3] [0..5])
-- UnsafeArray [3,4] [0,1,2,3,4,5]
unsafeModifyShape :: FromVector u Int => (u -> u) -> Array a -> Array a
unsafeModifyShape f (UnsafeArray s v) = UnsafeArray (asVector (f (vectorAs s))) v

-- | Unsafely modify an array vector
--
-- >>> unsafeModifyVector (V.map (+1)) (array [2,3] [0..5])
-- UnsafeArray [2,3] [1,2,3,4,5,6]
unsafeModifyVector :: FromVector u a => FromVector v b => (u -> v) -> Array a -> Array b
unsafeModifyVector f (UnsafeArray s v) = UnsafeArray s (asVector (f (vectorAs v)))

-- | Fill an array with the supplied value or cut the array values to match shape.
--
-- > validate (def x a) == True
--
-- >>> pretty $ def 0 (array [3] [])
-- [0,0,0]
-- >>> pretty $ def 0 (array [3] [1..4])
-- [1,2,3]
def :: a -> Array a -> Array a
def x (UnsafeArray s v) = UnsafeArray s (V.take (V.product s) (v <> V.replicate (V.product s - (V.length v)) x))

-- | shape of an Array
--
-- >>> shape a :: [Int]
-- [2,3,4]
--
-- >>> shape a :: Array Int
-- UnsafeArray [3] [2,3,4]
shape :: FromVector u Int => Array a -> u
shape (UnsafeArray s _) = vectorAs s

-- | rank of an Array
--
-- >>> rank a
-- 3
rank :: Array a -> Int
rank = V.length . shape

-- | size of an Array, which is the total number of elements, if the Array is valid.
--
-- >>> size a
-- 24
size :: Array a -> Int
size = V.product . shape

-- | Number of rows (first dimension size) in an Array. As a convention, a scalar value is still a single row.
--
-- >>> D.length a
-- 2
length :: Array a -> Int
length a = case shape a of
  [] -> one
  (x:_) -> x

-- | Is the Array empty, has zero number of elements.
--
-- >>> isNull ([2,0] >< [] :: Array ())
-- True
-- >>> isNull ([] >< [4] :: Array Int)
-- False
isNull :: Array a -> Bool
isNull = (zero==) . size

-- | An array with no elements. The shape ([0]) is somewhat arbitrary.
--
empty :: Array a
empty = array [0] []

flattenV :: V.Vector Int -> V.Vector Int -> Int
flattenV ns xs = V.sum $ V.zipWith (*) xs (V.drop 1 $ V.scanr (*) one ns)

shapenV :: V.Vector Int -> Int -> V.Vector Int
shapenV ns x = V.fromList $ S.shapenL (V.toList ns) x

-- | convert from a shape index to a flat index
--
-- >>> flatten [2,3,4] [1,1,1]
-- 17
--
-- >>> flatten [] [1,1,1]
-- 0
flatten :: (FromVector u Int) => u -> u -> Int
flatten ns xs = flattenV (asVector ns) (asVector xs)

-- | convert from a flat index to a shape index
--
-- >>> shapen [2,3,4] 17
-- [1,1,1]
shapen :: (FromVector u Int) => u -> Int -> u
shapen ns x = vectorAs $ shapenV (asVector ns) x

-- | shapenA is a variation of shapen that preserves the shape information of the shaped index. Practically this is needed to distinguish between a scalar and a 1-element array.
--
-- >>> shapenA (toScalar 3) 2
-- UnsafeArray [] [2]
-- >>> shapenA (array [1] [3]) 2
-- UnsafeArray [1] [2]
shapenA :: Array Int -> Int -> Array Int
shapenA ns x = UnsafeArray (shape ns) (shapenV (arrayAs ns) x)

-- | extract an element at index /i/
--
-- >>> index a [1,2,3]
-- 24
index :: (FromVector u Int) => Array a -> u -> a
index (UnsafeArray s v) i = V.unsafeIndex v (flatten s (asVector i))

-- | tabulate an array supplying a shape and a generating function
--
-- >>> tabulate [2,3,4] ((1+) . flatten [2,3,4]) == a
-- True
tabulate :: (FromVector u Int) => u -> (u -> a) -> Array a
tabulate ds f =
  UnsafeArray (asVector ds) (V.generate (V.product (asVector ds)) (\i -> f (shapen ds i)))

-- | tabulate an array with a generating function, with the shape supplied as an Array
--
-- In practice, the only difference is that the tabulator returns a scalar when a scalar is supplied.
--
-- >>> D.tabulate (D.toScalar 3) id
-- UnsafeArray [3] [UnsafeArray [1] [0],UnsafeArray [1] [1],UnsafeArray [1] [2]]
-- >>> D.tabulateA (D.toScalar 3) id
-- UnsafeArray [3] [UnsafeArray [] [0],UnsafeArray [] [1],UnsafeArray [] [2]]
tabulateA :: Array Int -> (Array Int -> a) -> Array a
tabulateA ds f =
  UnsafeArray (asVector ds) (V.generate (V.product (asVector ds)) (\i -> f (shapenA ds i)))

-- | This is a more general backpermute function than contained in https://benl.ouroborus.net/papers/2010-rarrays/repa-icfp2010.pdf which is similar to:
--
-- > backpermute f a = tabulate (f (shape a)) (index a . f)
--
-- where this version is:
--
-- > backpermute f g a = tabulate (f (shape a)) (index a . g)
--
-- Many of the functions here are examples of a backpermute (sans a little bit of bookkeeping), and, if you squint, you can see a fair few duals.
--
-- Some examples:
--
-- takeD d t ~ backpermute (modifyIndex d . min t) id
--
-- rotates rs ~ backpermute id (rotateIndex rs)
--
-- selectD d x ~ backpermute (S.deleteIndex d) (S.addIndex d x)
--
-- extracts ds ~ backpermute (S.takeIndexes ds) (S.addIndexes ds)
--
-- slice pss ~ backpermute (S.ranks pss) (List.zipWith (!!) pss)
--
-- reverses ds ~ backpermute id (S.reverseIndex ds)
--
-- transpose ~ backpermute List.reverse List.reverse
--
-- inflates ds n ~ backpermute (S.addIndexes ds n) (S.deleteIndexes ds)
--
-- Backpermuting operations are interesting as they do not involve actual inspection of the underlying elements, and so should be conducive to performance streaming benefits.
backpermute :: FromVector u Int => (u -> u) -> (u -> u) -> Array a -> Array a
backpermute f g a = tabulate (f (shape a)) (index a . g)

-- | backpermute lifted to a binary operation.
--
-- expand f ~ backpermute2 (<>) (bimap (List.take (rank a)) (List.drop (rank a))) f
backpermute2 :: FromVector u Int => ((u,u) -> u) -> (u -> (u,u)) -> (a -> b -> c) -> Array a -> Array b -> Array c
backpermute2 f g h a b =
  tabulate (f (shape a, shape b)) (uncurry h . bimap (index a) (index b) . g)

-- | Extract a value from a scalar. Unwrapping scalars is probably a performance bottleneck.
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

-- | convert scalars to dimensioned arrays
--
-- >>> asDimmed (toScalar 4)
-- UnsafeArray [1] [4]
asDimmed :: Array a -> Array a
asDimmed (UnsafeArray s v) = UnsafeArray (bool s (V.singleton 1) (V.null s)) v

-- | convert arrays with shape [1] to scalars
--
-- >>> asScalar (singleton 3)
-- UnsafeArray [] [3]
asScalar :: Array a -> Array a
asScalar (UnsafeArray s v) = UnsafeArray (bool s (V.empty) (s == V.singleton 1)) v

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

-- | Take the top-most elements across the specified dimension. Negative values take the bottom-most. No index check is performed.
--
-- > takeD d x == takes [(d,x)]
--
-- >>> pretty $ takeD 2 1 a
-- [[[1],
--   [5],
--   [9]],
--  [[13],
--   [17],
--   [21]]]
takeD ::
  Int ->
  Int ->
  Array a ->
  Array a
takeD d t a = tabulate dsNew $ \s -> index a (S.modifyIndex d (\x -> x + bool 0 (d'+t) (t<0)) s)
  where
    dsNew = S.modifyIndex d (\i -> min i (abs t)) (shape a)
    d' = shape a !! d

-- | A fold of takeD. May be slower than takes.
--
-- >>> pretty $ takeDs [(0,1), (1,2), (2,-3)] a
-- [[[2,3,4],
--   [6,7,8]]]
takeDs ::
  [(Int,Int)] ->
  Array a ->
  Array a
takeDs ts a = foldl' (\a' (d,t) -> takeD d t a') a ts

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

-- | Drop the top-most elements across the specified dimension. Negative values take the bottom-most.
--
-- >>> pretty $ dropD 2 2 a
-- [[[3,4],
--   [7,8],
--   [11,12]],
--  [[15,16],
--   [19,20],
--   [23,24]]]
dropD ::
  Int ->
  Int ->
  Array a ->
  Array a
dropD d t a = tabulate dsNew $ \s -> index a (S.modifyIndex d (\x -> x + bool t 0 (t<0)) s)
  where
    dsNew = S.replaceIndex d (d' - abs t) (shape a)
    d' = shape a !! d

-- | A fold of takeD. May be slower than takes.
--
-- >>> pretty $ dropDs [(0,1), (1,2), (2,-3)] a
-- [[[21]]]
dropDs ::
  [(Int,Int)] ->
  Array a ->
  Array a
dropDs ts a = foldl' (\a' (d,t) -> dropD d t a') a ts

-- | reshape an array, supplying a default value for elements outside the shape of the old array.
--
-- >>> reshapeDef 0 [5] (array [4] [0..3] :: Array Int)
-- UnsafeArray [5] [0,1,2,3,0]
reshapeDef ::
  (FromVector u Int) =>
  a ->
  u ->
  Array a ->
  Array a
reshapeDef d s' (UnsafeArray _ v) = def d (UnsafeArray (asVector s') v)

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

-- | Rotate an array according to dimension, length tuples.
--
-- >>> let x = array [2,3] [0..5] :: Array Int
-- >>> pretty $ rotates [(0,1),(1,1)] x
-- [[4,5,3],
--  [1,2,0]]
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

-- | Select by dimension & index.
--
-- >>> let s = selectD 2 3 a
-- >>> pretty s
-- [[4,8,12],
--  [16,20,24]]
selectD ::
  Int ->
  Int ->
  Array a ->
  Array a
selectD d x a = tabulate (S.deleteIndex (shape a) d) go
  where
    go s = index a (S.addIndex s d x)

-- | Select by (dimension,index) pairs.
--
-- >>> let s = selects [(0,1),(1,1)] a
-- >>> pretty s
-- [17,18,19,20]
selects ::
  (Foldable u, Functor u) =>
  u (Int, Int) ->
  Array a ->
  Array a
selects ds a = tabulate (S.deleteIndexes (shape a) ds') go
  where
    go s = index a (S.addIndexes s ds' xs)
    ds' = toList (fmap fst ds)
    xs = toList (fmap snd ds)

-- | Reduce along specified dimensions, using the supplied fold.
--
-- >>> pretty $ reduces [0] sum a
-- [[14,16,18,20],
--  [22,24,26,28],
--  [30,32,34,36]]
-- >>> pretty $ reduces [0,2] sum a
-- [68,100,132]
--
-- > \f x -> reduces [0..(rank x - 1)] f x == toScalar (f x)
reduces ::
  [Int] ->
  (Array a -> b) ->
  Array a ->
  Array b
reduces ds f a = tabulate (S.takeIndexes (shape a) ds') go
  where
    ds' = S.exclude (rank a) ds
    go s = f (selects (List.zip ds' s) a)

-- | Traverse along specified dimensions.
--
traverses ::
  (Applicative f) =>
  [Int] ->
  (a -> f b) ->
  Array a ->
  f (Array b)
traverses ds f a = fmap join $ sequenceA $ fmap (traverse f) (extracts ds a)

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

-- | Join inner and outer dimension layers by supplied dimensions. No checks on shape.
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
    go s = index (index a (S.takeIndexes s ds)) (S.deleteIndexes s ds)
    so = shape a
    si = shape (index a (replicate (rank a) 0))

-- | Join inner and outer dimension layers by supplied dimensions, check inner layer shape.
--
-- >>> let e = extracts [1,0] a
-- >>> (Right j) = joinsSafe [1,0] e
-- >>> a == j
-- True
joinsSafe ::
  [Int] ->
  Array (Array a) ->
  Either NumHaskException (Array a)
joinsSafe ds a =
  bool
    (Left $ NumHaskException "raggedy inner arrays")
    (Right $ joins ds a)
    (allEqual (fmap (shape @(Array Int)) a))

-- | Satisfy a predicate across all elements
allEqual :: (Eq a) => Array a -> Bool
allEqual a = case (arrayAs a) of
  [] -> True
  (x:xs) -> all (==x) xs

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

-- | Zips two arrays with a function along specified dimensions.
--
-- >>> pretty $ zips [0,1] (zipWithE (,)) a (reverses [0] a)
-- [[[(1,13),(2,14),(3,15),(4,16)],
--   [(5,17),(6,18),(7,19),(8,20)],
--   [(9,21),(10,22),(11,23),(12,24)]],
--  [[(13,1),(14,2),(15,3),(16,4)],
--   [(17,5),(18,6),(19,7),(20,8)],
--   [(21,9),(22,10),(23,11),(24,12)]]]
zips ::
  [Int] ->
  (Array a -> Array b -> Array c) ->
  Array a ->
  Array b ->
  Array c
zips ds f a b = joins ds (zipWithE f (extracts ds a) (extracts ds b))

-- | Zips two arrays with a function along specified dimensions, checking shapes.
--
-- >>> zipsSafe [0] (zipWithE (,)) (asArray [1::Int]) (asArray [1,2::Int])
-- Left (NumHaskException {errorMessage = "MisMatched zip"})
zipsSafe ::
  [Int] ->
  (Array a -> Array b -> Array c) ->
  Array a ->
  Array b ->
  Either NumHaskException (Array c)
zipsSafe ds f a b =
  bool
    (Right $ joins ds (zipWithE f (extracts ds a) (extracts ds b)))
    (Left (NumHaskException "MisMatched zip"))
    (shape a /= (shape b :: [Int]))

-- | apply a binary function between successive slices, across (dimension, lag) tuples
--
-- >>> pretty $ diffs [(1,1)] (zipWithE (-)) a
-- [[[4,4,4,4],
--   [4,4,4,4]],
--  [[4,4,4,4],
--   [4,4,4,4]]]
diffs :: [(Int, Int)] -> (Array a -> Array a -> Array b) -> Array a -> Array b
diffs ds f a = zips (fmap fst ds) f (drops ds a) (drops (fmap (second P.negate) ds) a)

-- | Concatenate along a dimension.
--
-- >>> shape @[Int] $ concatenate 1 a a
-- [2,6,4]
-- >>> concatenate 0 (toScalar 1) (toScalar 2)
-- UnsafeArray [2] [1,2]
-- >>> concatenate 0 (toScalar 0) (asArray [1..3])
-- UnsafeArray [4] [0,1,2,3]
concatenate ::
  Int ->
  Array a ->
  Array a ->
  Array a
concatenate d a0 a1 = tabulate (S.concatenate d (shape a0') (shape a1')) go
  where
    go s =
      bool
        (index a0' s)
        ( index
            a1
            ( S.addIndex
                (S.deleteIndex s d)
                d
                ((s !! d) - (ds0 !! d))
            )
        )
        ((s !! d) >= (ds0 !! d))
    ds0 = shape a0'
    a0' = asDimmed a0
    a1' = asDimmed a1

-- | Insert along a dimension at a position.
--
-- >>> pretty $ insert 2 0 a (array [2,3] [100..105])
-- [[[100,1,2,3,4],
--   [101,5,6,7,8],
--   [102,9,10,11,12]],
--  [[103,13,14,15,16],
--   [104,17,18,19,20],
--   [105,21,22,23,24]]]
-- >>> D.insert 0 0 (D.toScalar 1) (D.toScalar 2)
-- UnsafeArray [2] [2,1]
insert ::
  Int ->
  Int ->
  Array a ->
  Array a ->
  Array a
insert d i a b = tabulate (S.incAt d (shape (asDimmed a))) go
  where
    go s
      | s !! d == i = index (asDimmed b) (S.deleteIndex s d)
      | s !! d < i = index (asDimmed a) s
      | otherwise = index (asDimmed a) (S.decAt d s)

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

-- | Insert along a dimension at the beginning.
--
-- >>> pretty $ prepend 2 (array [2,3] [100..105]) a
-- [[[100,1,2,3,4],
--   [101,5,6,7,8],
--   [102,9,10,11,12]],
--  [[103,13,14,15,16],
--   [104,17,18,19,20],
--   [105,21,22,23,24]]]
prepend ::
  Int ->
  Array a ->
  Array a ->
  Array a
prepend d a b = insert d 0 b a

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
-- This generalises a tensor contraction by allowing the number of contracting diagonals to be other than 2.
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
    go s = index a (List.zipWith (!!) pss s)

-- | Slice along a dimension.
--
-- >>> let s = sliceD 2 1 2 a
-- >>> pretty s
-- [[[2,3],
--   [6,7],
--   [10,11]],
--  [[14,15],
--   [18,19],
--   [22,23]]]
sliceD ::
  Int ->
  Int ->
  Int ->
  Array a ->
  Array a
sliceD d offset l a = takes [(d,l)] $ drops [(d,offset)] a

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
reorder ds a = tabulate (S.reorder (shape a) ds) go
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

-- | Remove single dimensions.
--
-- >>> let a' = array [2,1,3,4,1] [1..24] :: Array Int
-- >>> shape @[Int] $ squeeze a'
-- [2,3,4]
squeeze ::
  Array a ->
  Array a
squeeze a = unsafeModifyShape (\s -> V.fromList $ S.squeeze (V.toList s)) a

-- | Insert a single dimension at the supplied position.
--
-- >>> shape @[Int] $ elongate 1 a
-- [2,1,3,4]
-- >>> elongate 0 (toScalar 1)
-- UnsafeArray [1] [1]
elongate ::
  Int ->
  Array a ->
  Array a
elongate d a = unsafeModifyShape (\s -> V.fromList $ S.addIndex (V.toList s) d 1) a

-- | Inflate an array by inserting a new dimension given a supplied dimension and size.
--
-- >>> pretty $ inflate 0 2 (array [3] [0,1,2])
-- [[0,1,2],
--  [0,1,2]]
inflate ::
  Int ->
  Int ->
  Array a ->
  Array a
inflate d n a = tabulate (S.addIndex (shape a) d n) (\s -> index a (S.deleteIndex s d))

-- | Inflate an array by inserting new dimensions given a (dimension, size) list.
--
-- >>> pretty $ inflates [(0,2)] (array [3] [0,1,2])
-- [[0,1,2],
--  [0,1,2]]
inflates ::
  [(Int,Int)] ->
  Array a ->
  Array a
inflates d a = tabulate (S.addIndexes (shape a) ds ns) (\s -> index a (S.deleteIndexes s ds))
  where
    ds = fmap fst d
    ns = fmap snd d

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

-- | Sort an array along the supplied dimensions.
--
-- >>> sorts [0] (array [2,2] [2,3,1,4])
-- UnsafeArray [2,2] [1,4,2,3]
-- >>> sorts [1] (array [2,2] [2,3,1,4])
-- UnsafeArray [2,2] [2,3,1,4]
-- >>> sorts [0,1] (array [2,2] [2,3,1,4])
-- UnsafeArray [2,2] [1,2,3,4]
sorts :: (Ord a) => [Int] -> Array a -> Array a
sorts ds a = joins ds $ unsafeModifyVector sortV (extracts ds a)

-- | The indices into the array if it were sorted by a comparison function along the dimensions supplied.
--
-- >>> import Data.Ord (Down (..))
-- >>> sortsBy [0] (fmap Down) (array [2,2] [2,3,1,4])
-- UnsafeArray [2,2] [2,3,1,4]
sortsBy :: (Ord b) => [Int] -> (Array a -> Array b) -> Array a -> Array a
sortsBy ds c a = joins ds $ unsafeModifyVector (sortByV c) (extracts ds a)

-- | The indices into the array if it were sorted along the dimensions supplied.
--
-- >>> orders [0] (array [2,2] [2,3,1,4])
-- UnsafeArray [2] [1,0]
orders :: (Ord a) => [Int] -> Array a -> Array Int
orders ds a = unsafeModifyVector orderV (extracts ds a)

-- | The indices into the array if it were sorted by a comparison function along the dimensions supplied.
--
-- >>> import Data.Ord (Down (..))
-- >>> ordersBy [0] (fmap Down) (array [2,2] [2,3,1,4])
-- UnsafeArray [2] [0,1]
ordersBy :: (Ord b) => [Int] -> (Array a -> Array b) -> Array a -> Array Int
ordersBy ds c a = unsafeModifyVector (orderByV c) (extracts ds a)


-- | Apply a binary array function to two arrays with matching shapes across the supplied dimensions. No check on shapes.
--
-- >>> a = D.array [2,3] [0..5]
-- >>> b = D.array [3] [0..2]
-- >>> pretty $ D.telecasts [(1,0)] (D.concatenate 0) a b
-- [[0,1,2],
--  [3,4,5],
--  [0,1,2]]
telecasts :: [(Int,Int)] -> (Array a -> Array b -> Array c) -> Array a -> Array b -> Array c
telecasts ds f a b = zipWithE f (extracts dsa a) (extracts dsb b) & joins dsa
  where
    dsb = fmap snd ds
    dsa = fmap fst ds

-- | Apply a binary array function to two arrays with matching shapes across the supplied dimensions. No check on shapes.
--
-- >>> a = D.array [2,3] [0..5]
-- >>> b = D.array [1] [1]
-- >>> telecastsSafe [(0,0)] (zipWithE (+)) a b
-- Left (NumHaskException {errorMessage = "MisMatched telecasting"})
telecastsSafe :: [(Int,Int)] -> (Array a -> Array b -> Array c) -> Array a -> Array b -> Either NumHaskException (Array c)
telecastsSafe ds f a b =
  bool
    (Right $ telecasts ds f a b)
    (Left (NumHaskException "MisMatched telecasting"))
    (shape (extracts dsa a) /= (shape (extracts dsb b) :: [Int]))
  where
    dsa = fmap fst ds
    dsb = fmap snd ds

-- | Apply a binary array function to two arrays where the shape of the first array is a prefix of the second array. No checks on shape.
--
-- >>> a = D.array [2,3] [0..5]
-- >>> pretty $ D.transmit (D.zipWithE (+)) (D.toScalar 1) a
-- [[1,2,3],
--  [4,5,6]]
transmit :: (Array a -> Array b -> Array c) -> Array a -> Array b -> Array c
transmit f a b = extractsExcept [0..rank a - 1] b & fmap (f a) & join

-- | Apply a binary array function to two arrays where the shape of the first array is a prefix of the second array. No checks on shape.
--
-- >>> a = D.array [2,3] [0..5]
-- >>> D.transmitSafe (D.zipWithE (+)) (array [3] [1,2,3]) a
-- Nothing
transmitSafe :: (Array a -> Array b -> Array c) -> Array a -> Array b -> Maybe (Array c)
transmitSafe f a b = bool Nothing (Just $ transmit f a b) ((shape a) `List.isPrefixOf` (shape b))

-- * row (first dimension) specializations

-- | selects specialised to selecting a single selection across the first dimension.
--
-- >>> pretty $ row 1 m
-- [4,5,6,7]
row :: Int -> Array a -> Array a
row i a = selects [(0,i)] a

-- | combine two arrays by rows
--
-- >>> pretty $ cons (array [2] [0,1]) (array [2,2] [2,3,4,5])
-- [[0,1],
--  [2,3],
--  [4,5]]
cons :: Array a -> Array a -> Array a
cons = prepend 0

-- | split an array into the first row and the remaining rows.
--
-- >>> uncons (array [3,2] [0..5])
-- (UnsafeArray [2] [0,1],UnsafeArray [2,2] [2,3,4,5])
uncons :: Array a -> (Array a, Array a)
uncons a = (selects [(0,0)] a, drops [(0,1)] a')
  where
    a' = bool a (UnsafeArray (V.singleton 1) (asVector a)) (isScalar a)

infix 5 :|

-- | cons :| uncons
--
-- >>> (x:|xs) = array [4] [0..3]
-- >>> x
-- UnsafeArray [] [0]
-- >>> xs
-- UnsafeArray [3] [1,2,3]
-- >>> (x:|xs)
-- UnsafeArray [4] [0,1,2,3]
pattern (:|) :: Array a -> Array a -> Array a
pattern x :| xs <- (uncons -> (x, xs))
  where
    x :| xs = cons x xs
{-# complete (:|) :: Array #-}

-- | take n rows, negative means take last n rows.
--
-- >>> D.take (-1) m
-- UnsafeArray [1,4] [8,9,10,11]
take :: Int -> Array a -> Array a
take n a = takes [(0,n)] a

-- | drop n rows, negative means drop last n rows.
--
-- >>> D.drop 2 m
-- UnsafeArray [1,4] [8,9,10,11]
drop :: Int -> Array a -> Array a
drop n a = drops [(0,n)] a

-- * column (last dimension) specizations

-- | selects specialised to selecting a single selection across the last dimension.
--
-- >>> pretty $ col 1 m
-- [1,5,9]
col :: Int -> Array a -> Array a
col i a = selects [(rank a - 1,i)] a

-- * element level specializations

-- | zip two arrays at an element level. Could also be called liftS2 or sometink like that.
--
-- > zipWithE == \f a b -> zips (iota (rank a)) (\f a b -> f (D.toScalar a) (D.toScalar b))
--
-- >>> zipWithE (-) v v
-- UnsafeArray [3] [0,0,0]
zipWithE :: (a -> b -> c) -> Array a -> Array b -> Array c
zipWithE f (UnsafeArray s v) (UnsafeArray _ v') = UnsafeArray s (V.zipWith f v v')

-- | zip two arrays at an element level, checking for shape consistency.
--
-- > zipWithE == \f a b -> zips (iota (rank a)) (\f a b -> f (D.toScalar a) (D.toScalar b))
--
-- >>> zipWithESafe (-) v (array [7] [0..6])
-- Left (NumHaskException {errorMessage = "Mismatched zip"})
zipWithESafe :: (a -> b -> c) -> Array a -> Array b -> Either NumHaskException (Array c)
zipWithESafe f (UnsafeArray s v) (UnsafeArray s' v') = bool (Left (NumHaskException "Mismatched zip")) (Right $ UnsafeArray s (V.zipWith f v v')) (s == s')

-- | difference a rank 1 array using the supplied function with a lag.
--
-- >>> pretty $ diff 1 (-) (iota [3,2])
-- [[2,2],
--  [2,2]]
diff :: Int -> (a -> a -> b) -> Array a -> Array b
diff n f a = zipWithE f (drop n a) (drop (-n) a)

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

-- | A flat enumeration
--
-- >>> pretty $ iota [2,3]
-- [[0,1,2],
--  [3,4,5]]
iota :: [Int] -> Array Int
iota xs = tabulate xs (flatten xs)

-- | Join inner and outer dimension layers in outer dimension order.
--
-- >>> a == join (extracts [0,1] a)
-- True
join ::
  Array (Array a) ->
  Array a
join a = joins [0..rank a - 1] a

-- | Join inner and outer dimension layers in outer dimension order, checking for consistent inner dimension shape
--
-- >>> joinSafe (extracts [0,1] a)
-- Right (UnsafeArray [2,3,4] [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24])
joinSafe ::
  Array (Array a) ->
  Either NumHaskException (Array a)
joinSafe a =
  bool
    (Left $ NumHaskException "raggedy inner arrays")
    (Right $ join a)
    (allEqual (fmap (shape @(Array Int)) a))

