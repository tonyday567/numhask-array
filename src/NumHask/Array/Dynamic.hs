{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

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

    -- * Shape interogation
    shape,
    rank,
    size,
    length,
    isNull,

    -- * indexing
    index,
    (!),
    (!?),
    tabulate,
    backpermute,

    -- * Scalar conversions
    fromScalar,
    toScalar,
    isScalar,
    asSingleton,
    asScalar,

    -- * Creation
    empty,
    range,
    indices,
    ident,
    konst,
    singleton,

    -- * operator generalisers
    rowWise,
    colWise,
    dimsWise,

    -- * uni-dimension operators
    take,
    drop,
    select,
    diag,
    undiag,
    concatenate,
    insert,
    delete,
    append,
    prepend,
    couple,
    slice,

    -- * multi-dimension operators
    -- ** selection
    takes,
    drops,
    indexes,
    heads,
    lasts,
    tails,
    inits,
    slices,

    -- ** function application
    reduces,
    traverses,
    extracts,
    extractsExcept,
    joins,
    joinsSafe,
    join,
    joinSafe,
    maps,
    filters,
    zips,
    zipsSafe,
    modifies,
    diffs,

    -- ** expansion
    expand,
    expandr,
    contract,
    dot,
    mult,
    windows,

    -- * search
    find,
    findNoOverlap,
    findIndices,
    isPrefixOf,
    isSuffixOf,
    isInfixOf,

    -- * shape manipulations
    fill,
    cut,
    cutSuffix,
    pad,
    lpad,
    repeat,
    cycle,
    rerank,
    reorder,
    reshape,
    flat,
    squeeze,
    elongate,
    rotate,
    transpose,
    inflate,
    intercalate,
    intersperse,
    reverses,
    concats,

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
    pattern (:<),
    cons,
    uncons,
    pattern (:>),
    snoc,
    unsnoc,

    -- * element-level specializations
    zipWithE,
    zipWithESafe,
    modifyE,
    diffE,
    imapE,

    -- * maths
    uniform,
    invtri,
    inverse,
    chol,
  )
where

import Control.Monad hiding (join)
import Data.Bifunctor
import Data.List qualified as List
import Data.Vector qualified as V
import NumHask.Array.Shape qualified as S
import NumHask.Array.Sort
import NumHask.Prelude as P hiding (cycle, diff, drop, empty, find, length, repeat, take, zip, zipWith)
import Prettyprinter hiding (dot, fill)
import System.Random hiding (uniform)
import System.Random.Stateful hiding (uniform)

-- $setup
-- >>> :m -Prelude
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude hiding (empty, diff, take, drop)
-- >>> import NumHask.Array.Dynamic as D
-- >>> import Prettyprinter hiding (dot, fill)
-- >>> import NumHask.Array.Shape qualified as S
-- >>> import Data.Vector qualified as V
-- >>> let s = 1 :: Array Int
-- >>> s
-- UnsafeArray [] [1]
-- >>> pretty s
-- 1
-- >>> let v = range [3]
-- >>> v
-- UnsafeArray [3] [0,1,2]
-- >>> let a = range [2,3,4]
-- >>> a
-- UnsafeArray [2,3,4] [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]
-- >>> pretty a
-- [[[0,1,2,3],
--   [4,5,6,7],
--   [8,9,10,11]],
--  [[12,13,14,15],
--   [16,17,18,19],
--   [20,21,22,23]]]

-- $usage
-- >>> :m -Prelude
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude hiding (empty, diff, take, drop)
-- >>> import NumHask.Array.Dynamic as D
-- >>> import Prettyprinter hiding (dot, fill)
-- >>> import NumHask.Array.Shape qualified as S
-- >>> import Data.Vector qualified as V
-- >>> let s = 1 :: Array Int
-- >>> s
-- UnsafeArray [] [1]
-- >>> pretty s
-- 1
-- >>> let a = range [2,3,4]
-- >>> a
-- UnsafeArray [2,3,4] [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]
-- >>> pretty a
-- [[[0,1,2,3],
--   [4,5,6,7],
--   [8,9,10,11]],
--  [[12,13,14,15],
--   [16,17,18,19],
--   [20,21,22,23]]]

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
data Array a = UnsafeArray [Int] (V.Vector a)
  deriving stock (Generic)
  deriving stock (Eq, Ord, Show)

instance Functor Array where
  fmap f = unsafeModifyVector (V.map f)

instance Foldable Array where
  foldr f x0 a = V.foldr f x0 (asVector a)

instance Traversable Array where
  traverse f (UnsafeArray s v) =
    array s <$> traverse f v

instance (Show a) => Pretty (Array a) where
  pretty a@(UnsafeArray _ v) = case rank a of
    0 -> viaShow (V.head v)
    1 -> viaShow v
    _ ->
      pretty "["
        <> indent
          0
          ( vsep
              ( punctuate comma $
                  pretty
                    <$> toList (extracts [0] a)
              )
          )
        <> pretty "]"

-- * conversions

instance (FromInteger a) => FromInteger (Array a) where
  fromInteger x = UnsafeArray [] (V.singleton (fromInteger x))

instance (FromRational a) => FromRational (Array a) where
  fromRational x = UnsafeArray [] (V.singleton (fromRational x))

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
  vectorAs v = UnsafeArray [V.length v] v

class FromArray t a | t -> a where
  asArray :: t -> Array a
  arrayAs :: Array a -> t

instance FromArray (Array a) a where
  asArray = id
  arrayAs = id

instance FromArray [a] a where
  asArray l = UnsafeArray [S.rank l] (V.fromList l)
  arrayAs (UnsafeArray _ v) = V.toList v

instance FromArray (V.Vector a) a where
  asArray v = UnsafeArray [V.length v] v
  arrayAs (UnsafeArray _ v) = v

-- | Polymorphic constructor of an array from shape and value vectors without any shape validation
--
-- >>> array [2,3,4] [0..23] == a
-- True
array :: (FromVector t a) => [Int] -> t -> Array a
array s (asVector -> v) = UnsafeArray s v

infixl 4 ><

-- | Construct an Array.
--
-- >>> pretty $ [2,3] >< [0..5]
-- [[0,1,2],
--  [3,4,5]]
(><) :: (FromVector t a) => [Int] -> t -> Array a
(><) = array

-- | Validate the size and shape of an array.
--
-- >>> validate (array [2,3,4] [1..23] :: Array Int)
-- False
validate :: Array a -> Bool
validate a = size a == V.length (asVector a)

-- | Construct an Array, checking shape.
--
-- >>> safeArray [2,3,4] [0..23] == Just a
-- True
safeArray :: (FromVector t a) => [Int] -> t -> Maybe (Array a)
safeArray s v =
  bool Nothing (Just a) (validate a)
  where
    a = UnsafeArray s (asVector v)

-- | Unsafely modify an array shape
--
-- >>> unsafeModifyShape (fmap (+1) :: [Int] -> [Int]) (array [2,3] [0..5])
-- UnsafeArray [3,4] [0,1,2,3,4,5]
unsafeModifyShape :: ([Int] -> [Int]) -> Array a -> Array a
unsafeModifyShape f (UnsafeArray s v) = UnsafeArray (f s) v

-- | Unsafely modify an array vector
--
-- >>> unsafeModifyVector (V.map (+1)) (array [2,3] [0..5])
-- UnsafeArray [2,3] [1,2,3,4,5,6]
unsafeModifyVector :: (FromVector u a) => (FromVector v b) => (u -> v) -> Array a -> Array b
unsafeModifyVector f (UnsafeArray s v) = UnsafeArray s (asVector (f (vectorAs v)))

-- | shape of an Array
--
-- >>> shape a
-- [2,3,4]
shape :: Array a -> [Int]
shape (UnsafeArray s _) = s

-- | rank of an Array
--
-- >>> rank a
-- 3
rank :: Array a -> Int
rank = List.length . shape

-- | size of an Array, which is the total number of elements, if the Array is valid.
--
-- >>> size a
-- 24
size :: Array a -> Int
size = S.size . shape

-- | Number of rows (first dimension size) in an Array. As a convention, a scalar value is still a single row.
--
-- >>> D.length a
-- 2
length :: Array a -> Int
length a = case shape a of
  [] -> one
  (x : _) -> x

-- | Is the Array empty, has zero number of elements.
--
-- >>> isNull ([2,0] >< [] :: Array ())
-- True
-- >>> isNull ([] >< [4] :: Array Int)
-- False
isNull :: Array a -> Bool
isNull = (zero ==) . size

-- | extract an element at index /i/
--
-- >>> index a [1,2,3]
-- 23
index :: Array a -> [Int] -> a
index (UnsafeArray s v) i = V.unsafeIndex v (S.flatten s i)

infixl 9 !

-- | extract an element at index /i/, unsafely
--
-- >>> a ! [1,2,3]
-- 23
(!) :: Array a -> [Int] -> a
(!) = index

-- | extract an element at index /i/, safely
--
-- >>> a !? [1,2,3]
-- Just 23
-- >>> a !? [2,3,1]
-- Nothing
(!?) :: Array a -> [Int] -> Maybe a
(!?) a xs = bool Nothing (Just (a ! xs)) (xs `S.inside` shape a)

-- | tabulate an array supplying a shape and a generating function
--
-- >>> tabulate [2,3,4] (S.flatten [2,3,4]) == a
-- True
tabulate :: [Int] -> ([Int] -> a) -> Array a
tabulate ds f =
  UnsafeArray ds (V.generate (V.product (asVector ds)) (f . S.shapen ds))

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
-- Backpermuting operations are interesting as they do not involve actual inspection of the underlying elements, and so should be conducive to performance streaming benefits.
--
-- > backpermute f g (backpermute f' g' a) == backpermute (f . f') (g . g') a
backpermute :: ([Int] -> [Int]) -> ([Int] -> [Int]) -> Array a -> Array a
backpermute f g a = tabulate (f (shape a)) (index a . g)
{-# INLINEABLE backpermute #-}

{- RULES
   "backpermute/backpermute" forall f f' g g' (a :: forall a. Array a)). backpermute f g (backpermute f' g' a) == backpermute (f . f') (g . g') a

-}

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
-- >>> asSingleton (toScalar 4)
-- UnsafeArray [1] [4]
asSingleton :: Array a -> Array a
asSingleton = unsafeModifyShape (\s -> bool s [1] (null s))

-- | convert arrays with shape [1] to scalars
--
-- >>> asScalar (singleton 3)
-- UnsafeArray [] [3]
asScalar :: Array a -> Array a
asScalar = unsafeModifyShape (\s -> bool s [] (s == [1]))

-- * Creation
-- | An array with no elements. The shape ([0]) is somewhat arbitrary.
--
-- >>> empty
-- UnsafeArray [0] []
empty :: Array a
empty = array [0] []

-- | A flat enumeration
--
-- >>> pretty $ range [2,3]
-- [[0,1,2],
--  [3,4,5]]
range :: [Int] -> Array Int
range xs = tabulate xs (S.flatten xs)

-- | Indices of an array shape.
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
ident ds = tabulate ds (bool zero one . S.isDiag . vectorAs . asVector)

-- | Create an array composed of a single value.
--
-- >>> pretty $ konst [3,2] one
-- [[1,1],
--  [1,1],
--  [1,1]]
konst :: [Int] -> a -> Array a
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
singleton a = UnsafeArray [1] (V.singleton a)

-- * operation generalisers

-- | apply a function that takes a [(dimension,parameter)] to a parameter list and the first dimensions.
--
-- >>> rowWise indexes [1,0] a
-- UnsafeArray [4] [12,13,14,15]
rowWise :: ([(Int, x)] -> Array a -> Array a) -> [x] -> Array a -> Array a
rowWise f xs a = f (List.zip [0 ..] xs) a

-- | apply a function that takes a [(dimension,parameter)] to a parameter list and the last dimensions (in reverse).
--
-- >>> colWise indexes [1,0] a
-- UnsafeArray [2] [1,13]
colWise :: ([(Int, x)] -> Array a -> Array a) -> [x] -> Array a -> Array a
colWise f xs a = f (List.zip (List.reverse [0 .. (rank a - 1)]) xs) a

-- | apply a function that takes a dimension and parameter, and modifies an array and folds a [(dimension,parameter)] list. In a perfect world, if the function is a backpermute, it should fuse.
--
-- >>> dimsWise take [(0,1),(2,2)] a
-- UnsafeArray [1,3,2] [0,1,4,5,8,9]
dimsWise :: (Int -> x -> Array a -> Array a) -> [(Int, x)] -> Array a -> Array a
dimsWise f xs a = foldl' (\a' (d, x) -> f d x a') a xs

-- * single dimension operators

-- | Take the top-most elements across the specified dimension. Negative values take the bottom-most. No index check is performed.
--
-- > take d x == takes [(d,x)]
--
-- >>> pretty $ take 2 1 a
-- [[[0],
--   [4],
--   [8]],
--  [[12],
--   [16],
--   [20]]]
take ::
  Int ->
  Int ->
  Array a ->
  Array a
take d t a = backpermute dsNew (S.modifyDim d (\x -> x + bool 0 (d' + t) (t < 0))) a
  where
    dsNew = S.modifyDim d (\i -> min i (abs t))
    d' = shape a !! d

-- | Drop the top-most elements across the specified dimension. Negative values take the bottom-most.
--
-- >>> pretty $ drop 2 2 a
-- [[[2,3],
--   [6,7],
--   [10,11]],
--  [[14,15],
--   [18,19],
--   [22,23]]]
drop ::
  Int ->
  Int ->
  Array a ->
  Array a
drop d t a = backpermute dsNew (S.modifyDim d (\x -> x + bool t 0 (t < 0))) a
  where
    dsNew = S.replaceDim d (d' - abs t)
    d' = shape a !! d

-- | Select an index along a dimension.
--
-- >>> let s = select 2 3 a
-- >>> pretty s
-- [[3,7,11],
--  [15,19,23]]
select ::
  Int ->
  Int ->
  Array a ->
  Array a
select d x a = backpermute (S.deleteDim d) (S.insertDim d x) a

-- | Extract the diagonal of an array.
--
-- >>> pretty $ diag (ident [3,2])
-- [1,1]
diag ::
  Array a ->
  Array a
diag a = backpermute minRank (replicate (rank a) . head) a
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

-- | Concatenate along a dimension.
--
-- >>> shape $ concatenate 1 a a
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
            ( S.insertDim
                d
                ((s !! d) - (ds0 !! d))
                (S.deleteDim d s)
            )
        )
        ((s !! d) >= (ds0 !! d))
    ds0 = shape a0'
    a0' = asSingleton a0
    a1' = asSingleton a1

-- | Insert along a dimension at a position.
--
-- >>> pretty $ insert 2 0 a (konst [2,3] 0)
-- [[[0,0,1,2,3],
--   [0,4,5,6,7],
--   [0,8,9,10,11]],
--  [[0,12,13,14,15],
--   [0,16,17,18,19],
--   [0,20,21,22,23]]]
-- >>> D.insert 0 0 (D.toScalar 1) (D.toScalar 2)
-- UnsafeArray [2] [2,1]
insert ::
  Int ->
  Int ->
  Array a ->
  Array a ->
  Array a
insert d i a b = tabulate (S.incAt d (shape (asSingleton a))) go
  where
    go s
      | s !! d == i = index (asSingleton b) (S.deleteDim d s)
      | s !! d < i = index (asSingleton a) s
      | otherwise = index (asSingleton a) (S.decAt d s)

-- | Delete along a dimension at a position.
--
-- >>> pretty $ delete 2 0 a
-- [[[0,1,2],
--   [4,5,6],
--   [8,9,10]],
--  [[12,13,14],
--   [16,17,18],
--   [20,21,22]]]
delete ::
  Int ->
  Int ->
  Array a ->
  Array a
delete d i a = backpermute (S.decAt d) (\s -> bool s (S.incAt d s) (s !! d < i)) (asSingleton a)

-- | Insert along a dimension at the end.
--
-- >>> pretty $ append 2 a (konst [2,3] 0)
-- [[[0,1,2,3,0],
--   [4,5,6,7,0],
--   [8,9,10,11,0]],
--  [[12,13,14,15,0],
--   [16,17,18,19,0],
--   [20,21,22,23,0]]]
append ::
  Int ->
  Array a ->
  Array a ->
  Array a
append d a b = insert d (S.indexOf d (shape a)) a b

-- | Insert along a dimension at the beginning.
--
-- >>> pretty $ prepend 2 (konst [2,3] 0) a
-- [[[0,0,1,2,3],
--   [0,4,5,6,7],
--   [0,8,9,10,11]],
--  [[0,12,13,14,15],
--   [0,16,17,18,19],
--   [0,20,21,22,23]]]
prepend ::
  Int ->
  Array a ->
  Array a ->
  Array a
prepend d a b = insert d 0 b a

-- | Combine two arrays as rows of a new array.
--
-- >>> pretty $ couple (asArray [1,2,3]) (asArray [4,5,6::Int])
-- [[1,2,3],
--  [4,5,6]]
couple :: Array a -> Array a -> Array a
couple a a' = concatenate 0 (elongate 0 a) (elongate 0 a')

-- | Slice along a dimension with the supplied (offset, length)
--
-- >>> let s = slice 2 (1,2) a
-- >>> pretty s
-- [[[1,2],
--   [5,6],
--   [9,10]],
--  [[13,14],
--   [17,18],
--   [21,22]]]
slice ::
  Int ->
  (Int, Int) ->
  Array a ->
  Array a
slice d (o, l) a = backpermute (S.replaceDim d l) (S.modifyDim d (+ o)) a

-- * multi-dimension operators

-- | Takes the top-most elements across the supplied dimensions. Negative values take the bottom-most.
--
-- > takes == dimsWise take
--
-- but is probably faster.
--
-- >>> pretty $ takes [(0,1), (1,2), (2,-3)] a
-- [[[1,2,3],
--   [5,6,7]]]
takes ::
  [(Int, Int)] ->
  Array a ->
  Array a
takes ts a = backpermute dsNew (List.zipWith3 (\d' a' s' -> bool s' (s' + a' + d') (d' < 0)) xsNew (shape a)) a
  where
    dsNew = S.replaceDims ds xsAbs
    xsNew = S.replaceDims ds xs (replicate (rank a) 0)
    ds = fmap fst ts
    xs = fmap snd ts
    xsAbs = fmap abs xs

-- | Drops the top-most elements. Negative values drop the bottom-most.
--
-- >>> pretty $ drops [(0,1), (1,2), (2,-3)] a
-- [[[20]]]
drops ::
  [(Int, Int)] ->
  Array a ->
  Array a
drops ts a = backpermute dsNew (List.zipWith (\d' s' -> bool (d' + s') s' (d' < 0)) xsNew) a
  where
    dsNew = S.modifyDims ds (fmap (flip (-)) xsAbs)
    xsNew = S.replaceDims ds xs (replicate (rank a) 0)
    ds = fmap fst ts
    xs = fmap snd ts
    xsAbs = fmap abs xs

-- | Rotate an array along a dimension.
--
-- >>> pretty $ rotate 1 2 a
-- [[[8,9,10,11],
--   [0,1,2,3],
--   [4,5,6,7]],
--  [[20,21,22,23],
--   [12,13,14,15],
--   [16,17,18,19]]]
rotate ::
  Int ->
  Int ->
  Array a ->
  Array a
rotate d r a = backpermute id (S.modifyDim d (\i -> (r + i) `mod` (shape a !! d))) a


-- | Select by (dimension,index) pairs.
--
-- >>> let s = indexes [(0,1),(1,1)] a
-- >>> pretty s
-- [16,17,18,19]
indexes ::
  [(Int, Int)] ->
  Array a ->
  Array a
indexes ds a = backpermute (S.deleteDims ds') (S.insertDims ds' xs) a
  where
    ds' = fmap fst ds
    xs = fmap snd ds

-- | Select the first element along the supplied dimensions
--
-- >>> pretty $ heads [0,2] a
-- [0,4,8]
heads :: [Int] -> Array a -> Array a
heads ds a = indexes (fmap (,0) ds) a

-- | Select the last element along the supplied dimensions
--
-- >>> pretty $ lasts [0,2] a
-- [15,19,23]
lasts :: [Int] -> Array a -> Array a
lasts ds a = indexes (List.zip ds lastds) a
  where
    lastds = (\i -> shape a !! i - 1) <$> ds

-- | Select the init elements along the supplied dimensions
--
-- >>> pretty $ tails [0,2] a
-- [[[13,14,15],
--   [17,18,19],
--   [21,22,23]]]
tails :: [Int] -> Array a -> Array a
tails ds a = slices (List.zip ds xs) a
  where
    xs = (1,) . (\i -> shape a !! i - 1) <$> ds

-- | Select the init elements along the supplied dimensions
--
-- >>> pretty $ inits [0,2] a
-- [[[0,1,2],
--   [4,5,6],
--   [8,9,10]]]
inits :: [Int] -> Array a -> Array a
inits ds a = slices (List.zip ds initds) a
  where
    initds = (0,) . (\i -> shape a !! i - 1) <$> ds

-- | Slices along dimensions.
--
-- >>> let s = slices [(2,(1,2)), (0,(1,1))] a
-- >>> pretty s
-- [[[13,14],
--   [17,18],
--   [21,22]]]
slices ::
  [(Int, (Int, Int))] ->
  Array a ->
  Array a
slices ps a = dimsWise slice ps a

-- * application

-- | Reduce along specified dimensions, using the supplied fold.
--
-- >>> pretty $ reduces [0] sum a
-- [66,210]
-- >>> pretty $ reduces [0,2] sum a
-- [[12,15,18,21],
--  [48,51,54,57]]
--
-- > \f x -> reduces [0..(rank x - 1)] f x == toScalar (f x)
reduces ::
  [Int] ->
  (Array a -> b) ->
  Array a ->
  Array b
reduces ds f a = fmap f (extracts ds a)

-- | Traverse along specified dimensions.
traverses ::
  (Applicative f) =>
  [Int] ->
  (a -> f b) ->
  Array a ->
  f (Array b)
traverses ds f a = join <$> traverse (traverse f) (extracts ds a)

-- | Extracts dimensions to an outer layer.
--
--
-- > a == (fromScalar <$> extracts [0..rank a] a)
--
-- >>> pretty $ shape <$> extracts [0] a
-- [[3,4],[3,4]]
extracts ::
  [Int] ->
  Array a ->
  Array (Array a)
extracts ds a = tabulate (S.takeDims ds (shape a)) go
  where
    go s = indexes (List.zip ds s) a

-- | Extracts /except/ dimensions to an outer layer.
--
-- >>> let e = extractsExcept [1,2] a
-- >>> pretty $ shape <$> extracts [0] a
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
joins ds a = tabulate (S.insertDims ds so si) go
  where
    go s = index (index a (S.takeDims ds s)) (S.deleteDims ds s)
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
    (allEqual (fmap shape a))

-- | Join inner and outer dimension layers in outer dimension order.
--
-- >>> a == join (extracts [0,1] a)
-- True
join ::
  Array (Array a) ->
  Array a
join a = joins [0 .. rank a - 1] a

-- | Join inner and outer dimension layers in outer dimension order, checking for consistent inner dimension shape
--
-- >>> joinSafe (extracts [0,1] a)
-- Right (UnsafeArray [2,3,4] [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23])
joinSafe ::
  Array (Array a) ->
  Either NumHaskException (Array a)
joinSafe a =
  bool
    (Left $ NumHaskException "raggedy inner arrays")
    (Right $ join a)
    (allEqual (fmap shape a))

-- | Satisfy a predicate across all elements
allEqual :: (Eq a) => Array a -> Bool
allEqual a = case arrayAs a of
  [] -> True
  (x : xs) -> all (== x) xs

-- | Maps a function along specified dimensions.
--
-- >>> shape $ maps [1] transpose a
-- [4,3,2]
maps ::
  [Int] ->
  (Array a -> Array b) ->
  Array a ->
  Array b
maps ds f a = joins ds (fmap f (extracts ds a))

-- | Filter along specified dimensions (which are flattened)
--
-- >>> pretty $ filters [0,1] (any ((==0) . (`mod` 7))) a
-- [[0,1,2,3],
--  [4,5,6,7],
--  [12,13,14,15],
--  [20,21,22,23]]
filters ::
  [Int] ->
  (Array a -> Bool) ->
  Array a ->
  Array a
filters ds p a = join (asArray $ V.filter p $ asVector (extracts ds a))

-- | Zips two arrays with a function along specified dimensions.
--
-- >>> pretty $ zips [0,1] (zipWithE (,)) a (reverses [0] a)
-- [[[(0,12),(1,13),(2,14),(3,15)],
--   [(4,16),(5,17),(6,18),(7,19)],
--   [(8,20),(9,21),(10,22),(11,23)]],
--  [[(12,0),(13,1),(14,2),(15,3)],
--   [(16,4),(17,5),(18,6),(19,7)],
--   [(20,8),(21,9),(22,10),(23,11)]]]
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

-- | Modify using the supplied function along dimension, positions.
--
-- >>> pretty $ modifies (fmap (100+)) [2] [0] a
-- [[[100,1,2,3],
--   [104,5,6,7],
--   [108,9,10,11]],
--  [[112,13,14,15],
--   [116,17,18,19],
--   [120,21,22,23]]]
modifies ::
  (Array a -> Array a) ->
  [Int] ->
  [Int] ->
  Array a ->
  Array a
modifies f ds xs a = joins ds $ modifyE xs f (extracts ds a)

-- | apply a binary function between successive slices, across (dimension, lag) tuples
--
-- >>> pretty $ diffs [(1,1)] (zipWithE (-)) a
-- [[[4,4,4,4],
--   [4,4,4,4]],
--  [[4,4,4,4],
--   [4,4,4,4]]]
diffs :: [(Int, Int)] -> (Array a -> Array a -> Array b) -> Array a -> Array b
diffs ds f a = zips (fmap fst ds) f (drops ds a) (drops (fmap (second P.negate) ds) a)

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
-- >>> x = array [3] [1,2,3]
-- >>> pretty $ expand (*) x x
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
-- [[(0,3),(0,4),(0,5)],
--  [(1,3),(1,4),(1,5)],
--  [(2,3),(2,4),(2,5)]]
--
-- >>> pretty $ expandr (,) v (fmap (+3) v)
-- [[(0,3),(1,3),(2,3)],
--  [(0,4),(1,4),(2,4)],
--  [(0,5),(1,5),(2,5)]]
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

-- | windows xs are xs-sized windows of an array
--
-- >>> D.shape $ D.windows [2,2] (D.range [4,3,2])
-- [3,2,2,2,2]
windows :: [Int] -> Array a -> Array a
windows xs a = backpermute df wf a
  where
    c = List.length xs
    df s = List.zipWith (\s' x' -> s' - x' + 1) s xs <> xs <> List.drop c s
    wf s = List.zipWith (+) (List.take c s) (List.take c (List.drop c s)) <> List.drop (c + c) s

-- * search

-- | find the starting positions of occurences of one array in another.
--
-- >>> a = D.cycle [4,4] (range [3]) :: Array Int
-- >>> i = array [2,2] [1,2,2,0] :: Array Int
-- >>> pretty $ D.find i a
-- [[False,True,False],
--  [True,False,False],
--  [False,False,True]]
find :: (Eq a) => Array a -> Array a -> Array Bool
find i a = xs
  where
    i' = rerank (rank a) i
    ws = windows (shape i') a
    xs = fmap (== i') (extracts (arrayAs (range [rank a]) <> [rank a * 2 .. (rank ws - 1)]) ws)

-- | find the ending positions of one array in another except where the array overlaps with another copy.
--
-- >>> a = D.konst [5,5] 1 :: Array Int
-- >>> i = D.konst [2,2] 1 :: Array Int
-- >>> pretty $ findNoOverlap i a
-- [[True,False,True,False],
--  [False,False,False,False],
--  [True,False,True,False],
--  [False,False,False,False]]
findNoOverlap :: (Eq a) => Array a -> Array a -> Array Bool
findNoOverlap i a = r
  where
    iexp = rerank (rank a) i
    f = find iexp a
    cl sh = List.filter (P.not . any (> 0) . List.init) $ List.filter (P.not . all (>= 0)) $ arrayAs $ tabulate ((\x -> 2 * x - 1) <$> sh) (\s -> List.zipWith (\x x0 -> x - x0 + 1) s sh)
    go r' s = index f s && all (P.not . index r') (List.filter (\x -> S.inside x (shape f)) $ fmap (List.zipWith (+) s) (cl (shape iexp)))
    r = tabulate (shape f) (go r)

-- | find the indices of the starting location of one array in another.
--
-- >>> b = D.cycle [4,4] (range [3]) :: Array Int
-- >>> i = array [2,2] [1,2,2,0] :: Array Int
-- >>> pretty $ D.findIndices i b
-- [[0,1],[1,0],[2,2]]
findIndices :: (Eq a) => Array a -> Array a -> Array [Int]
findIndices i a = fmap fst $ vectorAs $ V.filter snd $ asVector $ imapE (,) b
  where
    b = find i a

-- | Check if the first array is a prefix of the second
--
-- >>> isPrefixOf (array [2,2] [0,1,4,5]) a
-- True
isPrefixOf :: (Eq a) => Array a -> Array a -> Bool
isPrefixOf p a = p == cut (shape p) a

-- | Check if the first array is a suffix of the second
--
-- >>> isSuffixOf (array [2,2] [18,19,22,23]) a
-- True
isSuffixOf :: (Eq a) => Array a -> Array a -> Bool
isSuffixOf p a = p == cutSuffix (shape p) a

-- | Check if the first array is an infix of the second
--
-- >>> isInfixOf (array [2,2] [18,19,22,23]) a
-- True
isInfixOf :: (Eq a) => Array a -> Array a -> Bool
isInfixOf p a = or $ find p a

-- * shape manipulation
-- | Fill an array with the supplied value without regard to the original shape or cut the array values to match array size.
--
-- > validate (def x a) == True
--
-- >>> pretty $ fill 0 (array [3] [])
-- [0,0,0]
-- >>> pretty $ fill 0 (array [3] [1..4])
-- [1,2,3]
fill :: a -> Array a -> Array a
fill x (UnsafeArray s v) = UnsafeArray s (V.take (S.size s) (v <> V.replicate (S.size s - V.length v) x))

-- | cut an array to form a new (smaller) shape. Errors if the new shape is larger. The old array is reranked to the rank of the new shape first.
--
-- >>> cut [2] (array [4] [0..3] :: Array Int)
-- UnsafeArray [2] [0,1]
cut ::
  [Int] ->
  Array a ->
  Array a
cut s' a = bool (error "bad cut") (tabulate s' (index a'))  (and $ List.zipWith (<=) s' (shape a'))
  where
    a' = rerank (S.rank s') a

-- | cut an array to form a new (smaller) shape, using suffix elements. Errors if the new shape is larger. The old array is reranked to the rank of the new shape first.
--
-- >>> cutSuffix [2,2] a
-- UnsafeArray [2,2] [18,19,22,23]
cutSuffix ::
  [Int] ->
  Array a ->
  Array a
cutSuffix s' a = bool (error "bad cut") (tabulate s' (index a' . List.zipWith (+) diffDim))  (and $ List.zipWith (<=) s' (shape a'))
  where
    a' = rerank (S.rank s') a
    diffDim = List.zipWith (-) (shape a') s'

-- | pad an array to form a new shape, supplying a default value for elements outside the shape of the old array. The old array is reranked to the rank of the new shape first.
--
-- >>> pad 0 [5] (array [4] [0..3] :: Array Int)
-- UnsafeArray [5] [0,1,2,3,0]
pad ::
  a ->
  [Int] ->
  Array a ->
  Array a
pad d s' a = tabulate s' (\s -> bool d (index a' s) (s `S.inside` shape a'))
  where
    a' = rerank (S.rank s') a

-- | left pad an array to form a new shape, supplying a default value for elements outside the shape of the old array.
--
-- >>> lpad 0 [5] (array [4] [0..3] :: Array Int)
-- UnsafeArray [5] [0,0,1,2,3]
-- >>> pretty $ lpad 0 [3,3] (range [2,2] :: Array Int)
-- [[0,0,0],
--  [0,0,1],
--  [0,2,3]]
lpad ::
  a ->
  [Int] ->
  Array a ->
  Array a
lpad d s' a = tabulate s' (\s -> bool d (index a' (olds s)) (olds s `S.inside` shape a'))
  where
    a' = rerank (S.rank s') a
    gap = List.zipWith (-) s' (shape a')
    olds s = List.zipWith (-) s gap

-- | Reshape an array (with the same or less number of elements).
--
-- >>> pretty $ reshape [4,3,2] a
-- [[[0,1],
--   [2,3],
--   [4,5]],
--  [[6,7],
--   [8,9],
--   [10,11]],
--  [[12,13],
--   [14,15],
--   [16,17]],
--  [[18,19],
--   [20,21],
--   [22,23]]]
reshape ::
  [Int] ->
  Array a ->
  Array a
reshape s a = backpermute (const s) (S.shapen (shape a) . S.flatten s) a

-- Make an Array single dimensional
--
-- >>> pretty $ flat (range [2,2])
-- [0,1,2,3]
-- >>> pretty (flat $ toScalar 0)
-- [0]
flat :: Array a -> Array a
flat a = unsafeModifyShape (pure . S.size) a

-- | Reshape an array, repeating the original array. The shape of the array should be a suffix of the new shape.
--
-- >>> pretty $ D.repeat [2,2,2] (array [2] [1,2])
-- [[[1,2],
--   [1,2]],
--  [[1,2],
--   [1,2]]]
--
-- > D.repeat ds (toScalar x) == konst ds x
repeat ::
  [Int] ->
  Array a ->
  Array a
repeat s a = backpermute (const s) (List.drop (S.rank s - rank a)) a

-- | Reshape an array, cycliing through the elements withuot regard to the original shape.
--
-- >>> pretty $ D.cycle [2,2,2] (array [3] [1,2,3])
-- [[[1,2],
--   [3,1]],
--  [[2,3],
--   [1,2]]]
cycle ::
  [Int] ->
  Array a ->
  Array a
cycle s a = backpermute (const s) (S.shapen (shape a) . (`mod` (size a)) . S.flatten s) a


-- | Change rank by adding new dimenaions at the front, if the new rank is greater, or combining dimensions (from left to right) into rows, if the new rank is lower.
--
-- >>> shape (rerank 4 a)
-- [1,2,3,4]
-- >>> shape (rerank 2 a)
-- [6,4]
rerank :: Int -> Array a -> Array a
rerank r a = unsafeModifyShape (S.rerank r) a

-- | change the order of dimensions
--
-- >>> pretty $ reorder [2,0,1] a
-- [[[0,4,8],
--   [12,16,20]],
--  [[1,5,9],
--   [13,17,21]],
--  [[2,6,10],
--   [14,18,22]],
--  [[3,7,11],
--   [15,19,23]]]
reorder ::
  [Int] ->
  Array a ->
  Array a
reorder ds a = backpermute (`S.reorder` ds) (\s -> S.insertDims ds s []) a

-- | Remove single dimensions.
--
-- >>> let a' = array [2,1,3,4,1] [1..24] :: Array Int
-- >>> shape $ squeeze a'
-- [2,3,4]
squeeze ::
  Array a ->
  Array a
squeeze a = unsafeModifyShape S.squeeze a

-- | Insert a single dimension at the supplied position.
--
-- >>> shape $ elongate 1 a
-- [2,1,3,4]
-- >>> elongate 0 (toScalar 1)
-- UnsafeArray [1] [1]
elongate ::
  Int ->
  Array a ->
  Array a
elongate d a = unsafeModifyShape (S.insertDim d 1) a

-- | Inflate an array by inserting a new dimension given a supplied dimension and size.
--
-- alt name: replicate
--
-- >>> pretty $ inflate 0 2 (array [3] [0,1,2])
-- [[0,1,2],
--  [0,1,2]]
inflate ::
  Int ->
  Int ->
  Array a ->
  Array a
inflate d n a = backpermute (S.insertDim d n) (S.deleteDim d) a

-- | Reverse indices eg transposes the element A/ijk/ to A/kji/.
--
-- >>> index (transpose a) [1,0,0] == index a [0,0,1]
-- True
-- >>> pretty $ transpose (array [2,2,2] [1..8])
-- [[[1,5],
--   [3,7]],
--  [[2,6],
--   [4,8]]]
transpose :: Array a -> Array a
transpose a = backpermute List.reverse List.reverse a

-- | intercalate an array along dimensions
--
-- >>> pretty $ intercalate [2] (konst [2,3] 0) a
-- [[[0,0,1,0,2,0,3],
--   [4,0,5,0,6,0,7],
--   [8,0,9,0,10,0,11]],
--  [[12,0,13,0,14,0,15],
--   [16,0,17,0,18,0,19],
--   [20,0,21,0,22,0,23]]]
intercalate:: [Int] -> Array a -> Array a -> Array a
intercalate ds i a = joins ds $ asArray (List.intersperse i (arrayAs (extracts ds a)))

-- | intersperse an element along dimensions
--
-- >>> pretty $ intersperse [2] 0 a
-- [[[0,0,1,0,2,0,3],
--   [4,0,5,0,6,0,7],
--   [8,0,9,0,10,0,11]],
--  [[12,0,13,0,14,0,15],
--   [16,0,17,0,18,0,19],
--   [20,0,21,0,22,0,23]]]
intersperse :: [Int] -> a -> Array a -> Array a
intersperse ds i a = intercalate ds (konst (S.deleteDims ds (shape a)) i) a

-- | Concatenate and replace dimensions, creating a new dimension at the supplied postion.
--
-- >>> pretty $ concats [0,1] 1 a
-- [[0,4,8,12,16,20],
--  [1,5,9,13,17,21],
--  [2,6,10,14,18,22],
--  [3,7,11,15,19,23]]
concats ::
  [Int] ->
  Int ->
  Array a ->
  Array a
concats ds n a = backpermute concatDims unconcatDims a
  where
    concatDims s = S.insertDim n (S.size $ S.takeDims ds s) (S.deleteDims ds s)
    unconcatDims s = S.insertDims ds (S.shapen (S.takeDims ds (shape a)) (S.indexOf n s)) (S.deleteDim n s)

-- | reverses order along specified dimensions.
--
-- >>> pretty $ reverses [0,1] a
-- [[[20,21,22,23],
--   [16,17,18,19],
--   [12,13,14,15]],
--  [[8,9,10,11],
--   [4,5,6,7],
--   [0,1,2,3]]]
reverses ::
  [Int] ->
  Array a ->
  Array a
reverses ds a = backpermute id (S.reverseIndex ds (shape a)) a


-- * sorting

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

-- * transmission

-- | Apply a binary array function to two arrays with matching shapes across the supplied dimensions. No check on shapes.
--
-- >>> a = D.array [2,3] [0..5]
-- >>> b = D.array [3] [0..2]
-- >>> pretty $ D.telecasts [(1,0)] (D.concatenate 0) a b
-- [[0,1,2],
--  [3,4,5],
--  [0,1,2]]
telecasts :: [(Int, Int)] -> (Array a -> Array b -> Array c) -> Array a -> Array b -> Array c
telecasts ds f a b = zipWithE f (extracts dsa a) (extracts dsb b) & joins dsa
  where
    dsb = fmap snd ds
    dsa = fmap fst ds

-- | Apply a binary array function to two arrays with matching shapes across the supplied dimensions. Checks shape.
--
-- >>> a = D.array [2,3] [0..5]
-- >>> b = D.array [1] [1]
-- >>> telecastsSafe [(0,0)] (zipWithE (+)) a b
-- Left (NumHaskException {errorMessage = "MisMatched telecasting"})
telecastsSafe :: [(Int, Int)] -> (Array a -> Array b -> Array c) -> Array a -> Array b -> Either NumHaskException (Array c)
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
transmit f a b = maps ds (f a) b
  where
    ds = [(rank a) .. (rank b - 1)]

-- | Apply a binary array function to two arrays where the shape of the first array is a prefix of the second array. Checks shape.
--
-- >>> a = D.array [2,3] [0..5]
-- >>> D.transmitSafe (D.zipWithE (+)) (array [3] [1,2,3]) a
-- Nothing
transmitSafe :: (Array a -> Array b -> Array c) -> Array a -> Array b -> Maybe (Array c)
transmitSafe f a b = bool Nothing (Just $ transmit f a b) (shape a `List.isPrefixOf` shape b)

-- * row (first dimension) specializations

-- | Add a new row
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
uncons a = (heads [0] a', tails [0] a')
  where
    a' = asSingleton a


-- | cons :< uncons
--
-- >>> (x:<xs) = array [4] [0..3]
-- >>> x
-- UnsafeArray [] [0]
-- >>> xs
-- UnsafeArray [3] [1,2,3]
-- >>> (x:<xs)
-- UnsafeArray [4] [0,1,2,3]
pattern (:<) :: Array a -> Array a -> Array a
pattern x :< xs <- (uncons -> (x, xs))
  where
    x :< xs = cons x xs

infix 5 :<

{-# COMPLETE (:<) :: Array #-}

-- | Add a new row at the end
--
-- >>> pretty $ snoc (array [2,2] [0,1,2,3]) (array [2] [4,5])
-- [[0,1],
--  [2,3],
--  [4,5]]
snoc :: Array a -> Array a -> Array a
snoc = append 0

-- | split an array into the initial rows and the last row.
--
-- >>> unsnoc (array [3,2] [0..5])
-- (UnsafeArray [2,2] [0,1,2,3],UnsafeArray [2] [4,5])
unsnoc :: Array a -> (Array a, Array a)
unsnoc a = (inits [0] a', lasts [0] a')
  where
    a' = asSingleton a

-- | unsnoc |: snoc
--
-- >>> (xs:>x) = array [4] [0..3]
-- >>> x
-- UnsafeArray [] [3]
-- >>> xs
-- UnsafeArray [3] [0,1,2]
-- >>> (xs:>x)
-- UnsafeArray [4] [0,1,2,3]
pattern (:>) :: Array a -> Array a -> Array a
pattern xs :> x <- (unsnoc -> (xs, x))
  where
    xs :> x = snoc xs x

infix 5 :>

{-# COMPLETE (:>) :: Array #-}

-- * element level specializations

-- | zip two arrays at an element level. Could also be called liftS2 or sometink like that.
--
-- > zipWithE == \f a b -> zips (range (rank a)) (\f a b -> f (D.toScalar a) (D.toScalar b))
--
-- >>> zipWithE (-) v v
-- UnsafeArray [3] [0,0,0]
zipWithE :: (a -> b -> c) -> Array a -> Array b -> Array c
zipWithE f (UnsafeArray s v) (UnsafeArray _ v') = UnsafeArray s (V.zipWith f v v')

-- | zip two arrays at an element level, checking for shape consistency.
--
-- > zipWithE == \f a b -> zips (range (rank a)) (\f a b -> f (D.toScalar a) (D.toScalar b))
--
-- >>> zipWithESafe (-) v (array [7] [0..6])
-- Left (NumHaskException {errorMessage = "Mismatched zip"})
zipWithESafe :: (a -> b -> c) -> Array a -> Array b -> Either NumHaskException (Array c)
zipWithESafe f (UnsafeArray s v) (UnsafeArray s' v') = bool (Left (NumHaskException "Mismatched zip")) (Right $ UnsafeArray s (V.zipWith f v v')) (s == s')

-- | row-wise difference an array using the supplied function with a lag.
--
-- >>> pretty $ diffE 1 (-) (range [3,2])
-- [[2,2],
--  [2,2]]
diffE :: Int -> (a -> a -> b) -> Array a -> Array b
diffE n f a = zipWithE f (rowWise (dimsWise drop) [n] a) (rowWise (dimsWise drop) [-n] a)

-- | modify a value at an index
--
-- >>> pretty $ modifyE [0,0] (const 100) (range [3,2])
-- [[100,1],
--  [2,3],
--  [4,5]]
modifyE :: [Int] -> (a -> a) -> Array a -> Array a
modifyE ds f a = tabulate (shape a) (\s -> bool id f (s == ds) (index a s))

-- | Maps an index function at element-level.
--
-- >>> pretty $ imapE (\xs x -> x - sum xs) a
-- [[[0,0,0,0],
--   [3,3,3,3],
--   [6,6,6,6]],
--  [[11,11,11,11],
--   [14,14,14,14],
--   [17,17,17,17]]]
imapE ::
  ([Int] -> a -> b) ->
  Array a ->
  Array b
imapE f a = zipWithE f (indices (shape a)) a

-- * Math

-- | Generate an array of uniform random variates between a range.
--
-- >>> import System.Random.Stateful hiding (uniform)
-- >>> g <- newIOGenM (mkStdGen 42)
-- >>> u <- uniform g [2,3,4] (0,9 :: Int)
-- >>> pretty u
-- [[[0,7,0,2],
--   [1,7,4,2],
--   [5,9,8,2]],
--  [[9,8,1,0],
--   [2,2,8,2],
--   [2,8,0,6]]]
uniform :: (StatefulGen g m, UniformRange a) => g -> [Int] -> (a,a) -> m (Array a)
uniform g ds r = do
  v <- V.replicateM (S.size ds) (uniformRM r g)
  pure $ UnsafeArray ds v

-- | Inverse of a square matrix.
--
-- >>> e = D.array [3,3] [4,12,-16,12,37,-43,-16,-43,98] :: D.Array Double
-- >>> pretty (inverse e)
-- [[49.36111111111111,-13.555555555555554,2.1111111111111107],
--  [-13.555555555555554,3.7777777777777772,-0.5555555555555555],
--  [2.1111111111111107,-0.5555555555555555,0.1111111111111111]]
--
-- > D.mult (D.inverse a) a == a
inverse :: (ExpField a) => Array a -> Array a
inverse a = mult (invtri (transpose (chol a))) (invtri (chol a))

-- | Inversion of a Triangular Matrix
 -- <https://math.stackexchange.com/questions/1003801/inverse-of-an-invertible-upper-triangular-matrix-of-order-3 Inverse of a triangular> matrix.
--
-- >>> t = array [3,3] ([1,0,1,0,1,2,0,0,1] :: [Double]) :: Array Double
-- >>> pretty (invtri t)
-- [[1.0,0.0,-1.0],
--  [0.0,1.0,-2.0],
--  [0.0,0.0,1.0]]
-- >>> ident (shape t) == mult t (invtri t)
-- True
invtri :: (ExpField a) => Array a -> Array a
invtri a = i
  where
    ti = undiag 2 (fmap recip (diag a))
    tl = zipWithE (-) a (undiag 2 (diag a))
    l = fmap negate (dot sum (*) ti tl)
    pow xs x = foldr ($) (ident (shape xs)) (replicate x (mult xs))
    zero' = konst (shape a) zero
    add = zipWithE (+)
    sum' = foldl' add zero'
    i = mult (sum' (fmap (pow l) (fmap fromIntegral $ range [n]))) ti
    n = shape a !! 0

-- | cholesky decomposition
--
-- Uses the <https://en.wikipedia.org/wiki/Cholesky_decomposition#The_Cholesky_algorithm Cholesky-Crout> algorithm.
--
-- >>> e = D.array [3,3] [4,12,-16,12,37,-43,-16,-43,98] :: D.Array Double
-- >>> pretty (D.chol e)
-- [[2.0,0.0,0.0],
--  [6.0,1.0,0.0],
--  [-8.0,5.0,3.0]]
-- >>> D.mult (D.chol e) (D.transpose (D.chol e)) == e
-- True
chol :: (ExpField a) => Array a -> Array a
chol a =
  let l =
        tabulate (shape a)
          ( \[i, j] ->
              bool
                ( one
                    / index l [j, j]
                    * ( index a [i, j]
                          - sum
                            ( (\k -> index l [i, k] * index l [j, k])
                                <$> [zero .. (j - one)]
                            )
                      )
                )
                ( sqrt
                    ( index a [i, i]
                        - sum
                          ( (\k -> index l [j, k] ^ 2)
                              <$> [zero .. (j - one)]
                          )
                    )
                )
                (i == j)
          )
   in l


-- * numeric broadcasting
broadOp :: (a -> b -> c) -> Array a -> Array b -> Array c
broadOp f a b
  | shape a == shape b = zipWithE f a b
  | (shape a) `List.isPrefixOf` (shape b) = transmit (zipWithE f) a b
  | (shape b) `List.isPrefixOf` (shape a) = transmit (zipWithE (flip f)) b a
  | otherwise = error "bad shapes"

instance
  ( Additive a
  ) =>
  Additive (Array a)
  where
  (+) = broadOp (+)

  zero = toScalar zero

instance
  ( Subtractive a
  ) =>
  Subtractive (Array a)
  where
  negate = fmap negate
