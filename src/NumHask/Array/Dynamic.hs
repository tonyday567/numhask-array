{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Arrays with a dynamic shape (shape only known at runtime).
module NumHask.Array.Dynamic
  ( -- $usage
    Array (..),
    pattern (:|),
    -- * Conversion
    fromFlatList,
    toFlatList,
    fromList1,
    fromList2,

    -- * representable replacements
    index,
    tabulate,

    -- * Operators
    isEmpty,
    takes,
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
    drops',
    liftR2_,
    transpose,
    order,
    orderBy
  )
where

import Data.List qualified as List
import Data.Vector qualified as V
import GHC.Show (Show (..))
import NumHask.Array.Shape
import NumHask.Prelude as P
import Data.Bifunctor qualified as B
import NumHask.Array.Sort

-- $setup
-- >>> :m -Prelude
-- >>> :set -XDataKinds
-- >>> :set -XOverloadedLists
-- >>> :set -XTypeFamilies
-- >>> :set -XFlexibleContexts
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude
-- >>> import NumHask.Array.Dynamic
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
-- >>> let s = fromFlatList [] [1] :: Array Int
-- >>> let a = fromFlatList [2,3,4] [1..24] :: Array Int
-- >>> let v = fromFlatList [3] [1,2,3] :: Array Int
-- >>> let m = fromFlatList [3,4] [0..11] :: Array Int


data NumHaskWarning =
    NYI | EmptyStack1 | EmptyStack2 | ApplyFunction | NotBox | TypeMismatch | SizeMismatch | NotNat | EmptyArray | NotArray deriving (Eq, Ord, Show)

-- | a multidimensional array with a value-level shape
--
-- >>> let a = fromFlatList [2,3,4] [1..24] :: Array Int
-- >>> a
-- [[[1, 2, 3, 4],
--   [5, 6, 7, 8],
--   [9, 10, 11, 12]],
--  [[13, 14, 15, 16],
--   [17, 18, 19, 20],
--   [21, 22, 23, 24]]]
data Array a = Array {shapeList :: [Int], unArray :: V.Vector a}
  deriving (Eq, Ord, Generic)

instance Functor Array where
  fmap f (Array s a) = Array s (V.map f a)

instance Foldable Array where
  foldr x a (Array _ v) = V.foldr x a v

instance Traversable Array where
  traverse f (Array s v) =
    fromFlatList s <$> traverse f (toList v)

instance (Show a) => Show (Array a) where
  show a@(Array l _) = go (P.length l) a
    where
      go n a'@(Array l' m) =
        case P.length l' of
          0 -> GHC.Show.show (V.head m)
          1 -> "[" ++ List.intercalate ", " (GHC.Show.show <$> V.toList m) ++ "]"
          x ->
            "["
              ++ List.intercalate
                (",\n" ++ replicate (n - x + 1) ' ')
                (go n <$> snd (toFlatList (extracts [0] a')))
              ++ "]"

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
fromFlatList ds l = Array ds $ V.fromList $ List.take (size ds) l

-- | convert to a (shape, (1-D) list) tuple.
--
-- >>> toFlatList a == ([2,3,4],[1..24])
-- True
--
-- >>> fromFlatList
toFlatList :: Array a -> ([Int], [a])
toFlatList (Array s v) = (s, V.toList v)

fromList1 :: [a] -> Array a
fromList1 xs = fromFlatList [P.length xs] xs

fromList2 :: Int -> [a] -> Array a
fromList2 r xs = fromFlatList [r, P.length xs `div` r] xs

-- | arbitrary levels
-- >>> flatten' ([[[1,2,3],[4,5,6]],[[7,8,9],[10,11,12]]] :: [[[Int]]]) :: [Int]
-- [1,2,3,4,5,6,7,8,9,10,11,12]
class Flatten i o where
  flatten' :: [i] -> [o]

instance Flatten a a where
  flatten' = id

instance Flatten i o => Flatten [i] o where
  flatten' = concatMap flatten'

-- |
-- >>> fst (shapen' ([[4,5,6],[1,2,3]] :: [[Int]]) :: ([Int], Int))
-- [2,3]
class Shapen i o where
  shapen' :: [i] -> ([Int],o)

instance Shapen a a where
  shapen' x = ([P.length x], head x)

instance Shapen i o => Shapen [i] o where
  shapen' x = B.first ([P.length x]<>) $ shapen' (head x)

-- | arbitrary levels
--
-- >>> unnest ([[4,5,6],[1,2,3]] :: [[Int]]) :: Either String ([Int], [Int])
-- Right ([2,3],[4,5,6,1,2,3])
class UnNest i o where
  unnest :: [i] -> Either String ([Int], [o])

instance UnNest a a where
  unnest x = Right ([P.length x], x)

instance UnNest i o => UnNest [i] o where
  unnest x = case errs of
    [] -> (,xs') . ([P.length x]<>)<$> eql
    (e:_) -> Left e
    where
      x2 = fmap unnest x
      errs = [e | (Left e) <- x2]
      eql = let (h:ls) = [l | (Right (l,_)) <- x2] in bool (Left "Raggedy list") (Right h) (all (h==) ls)
      xs' = mconcat [ns | (Right (_,ns)) <- x2]

-- FIXME: probably the best to hope for for nesting a flat list
-- (groupN 1 <$> groupN 3 [4,5,6,1,2,3])
-- >>> nest ([2,3],[4,5,6,1,2,3])
groupN :: Int -> [a] -> [[a]]
groupN _ [] = []
groupN n xs = List.take n xs : groupN n (List.drop n xs)

-- | arbitrary levels
--
-- >>> unnest ([[4,5,6],[1,2,3]] :: [[Int]]) :: Either String ([Int], [Int])
-- Right ([2,3],[4,5,6,1,2,3])
class Nest i o where
  nest :: [Int] -> i -> o

instance Nest [i] [i] where
  nest [] x = x

instance Nest [Int] [[Int]] where
  nest (_:ss) x = nest ss <$> groupN (foldl' (*) 1 ss) x

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

liftR2 :: (a -> b -> c) -> Array a -> Array b -> Either String (Array c)
liftR2 f x x' = bool (Left "shape mismatch") (Right $ Array (shapeList x) (V.zipWith f (unArray x) (unArray x'))) (shapeList x == shapeList x')

liftR2_ :: (a -> b -> c) -> Array a -> Array b -> Array c
liftR2_ f x x' = either error id (liftR2 f x x')

isEmpty :: Array a -> Bool
isEmpty = (zero==) . size . shapeList

-- | Takes the top-most elements according to the new dimension.
--
-- >>> takes [2,2,3] a
-- [[[1, 2, 3],
--   [5, 6, 7]],
--  [[13, 14, 15],
--   [17, 18, 19]]]
takes ::
  [Int] ->
  Array a ->
  Array a
takes ds a = tabulate ds $ \s -> index a s

-- | Takes the top-most elements according to the new dimension. Negative values take the bottom-most.
--
-- >>> takes' [2,2,(-3)] a
-- [[[1, 2, 3],
--   [5, 6, 7]],
--  [[13, 14, 15],
--   [17, 18, 19]]]
takes' ::
  [Int] ->
  Array a ->
  Array a
takes' ds a = tabulate ds' $ \s -> index a (zipWith3 (\d' s' a' -> bool s' (s' + a' + d') (d'<0)) ds s (shapeList a))
  where ds' = fmap abs ds

-- | Drops the top-most elements. Negative values drop the bottom-most.
--
-- >>> drops' [1,2,(-3)] a
drops' ::
  [Int] ->
  Array a ->
  Array a
drops' ds a = tabulate dsNew $ \s -> index a (zipWith (\d' s' -> bool (d' + s') s' (d'<0)) ds s)
  where
    ds' = fmap abs ds
    dsNew = zipWith (\x d -> max 0 (x - d)) (shapeList a) ds'


-- | Reshape an array (with the same number of elements).
--
-- >>> reshape' [4,3,2] a
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
reshape' ::
  [Int] ->
  Array a ->
  Array a
reshape' s a = tabulate s (index a . shapen (shapeList a) . flatten s)

rotates ::
  [(Int,Int)] ->
  Array a ->
  Array a
rotates rs a = tabulate (shapeList a) (index a . rotateIndex rs (shapeList a))

-- | Indices of an Array.
--
-- >>> indices [3,3]
-- [[[0,0], [0,1], [0,2]],
--  [[1,0], [1,1], [1,2]],
--  [[2,0], [2,1], [2,2]]]
indices :: [Int] -> Array [Int]
indices ds = tabulate ds id

-- | The identity array.
--
-- >>> ident [3,2]
-- [[1, 0],
--  [0, 1],
--  [0, 0]]
ident :: (Additive a, Multiplicative a) => [Int] -> Array a
ident ds = tabulate ds (bool zero one . isDiag)
  where
    isDiag [] = True
    isDiag [_] = True
    isDiag [x, y] = x == y
    isDiag (x : y : xs) = x == y && isDiag (y : xs)

-- | An array of sequential Ints
--
-- >>> sequent [3]
-- [0, 1, 2]
--
-- >>> sequent [3,3]
-- [[0, 0, 0],
--  [0, 1, 0],
--  [0, 0, 2]]
sequent :: [Int] -> Array Int
sequent ds = tabulate ds go
  where
    go [] = zero
    go [i] = i
    go (i : js) = bool zero i (all (i ==) js)

-- | Extract the diagonal of an array.
--
-- >>> diag (ident [3,2])
-- [1, 1]
diag ::
  Array a ->
  Array a
diag a = tabulate [NumHask.Array.Shape.minimum (shapeList a)] go
  where
    go [] = throw (NumHaskException "Rank Underflow")
    go (s' : _) = index a (replicate (rank (shapeList a)) s')

-- | Expand the array to form a diagonal array
--
-- >>> undiag 2 (fromFlatList [2] [1,1])
-- [[1, 0],
--  [0, 1]]
undiag ::
  (Additive a) =>
  Int ->
  Array a ->
  Array a
undiag r a = tabulate (replicate r (head (shapeList a))) go
  where
    go [] = throw (NumHaskException "Rank Underflow")
    go xs@(x : xs') = bool zero (index a xs) (all (x ==) xs')


-- | Create an array composed of a single value.
--
-- >>> singleton [3,2] one
-- [[1, 1],
--  [1, 1],
--  [1, 1]]
single :: [Int] -> a -> Array a
single ds a = tabulate ds (const a)

-- | Create an array of dimension [1] composed of a single value.
--
-- >>> singleton_ [3,2] one
-- [[1, 1],
--  [1, 1],
--  [1, 1]]
singleton :: a -> Array a
singleton a = Array [1] (V.singleton a)

-- | Select an array along dimensions.
--
-- >>> let s = selects [0,1] [1,1] a
-- >>> s
-- [17, 18, 19, 20]
selects ::
  [Int] ->
  [Int] ->
  Array a ->
  Array a
selects ds i a = tabulate (dropIndexes (shapeList a) ds) go
  where
    go s = index a (addIndexes s ds i)

-- | Select an index /except/ along specified dimensions
--
-- >>> let s = selectsExcept [2] [1,1] a
-- >>> s
-- [17, 18, 19, 20]
selectsExcept ::
  [Int] ->
  [Int] ->
  Array a ->
  Array a
selectsExcept ds i a = selects (exclude (rank (shapeList a)) ds) i a

-- | Fold along specified dimensions.
--
-- >>> folds sum [1] a
-- [68, 100, 132]
folds ::
  (Array a -> b) ->
  [Int] ->
  Array a ->
  Array b
folds f ds a = tabulate (takeIndexes (shapeList a) ds) go
  where
    go s = f (selects ds s a)

-- | Extracts dimensions to an outer layer.
--
-- >>> let e = extracts [1,2] a
-- >>> shapeList <$> extracts [0] a
-- [[3,4], [3,4]]
extracts ::
  [Int] ->
  Array a ->
  Array (Array a)
extracts ds a = tabulate (takeIndexes (shapeList a) ds) go
  where
    go s = selects ds s a

-- | Extracts /except/ dimensions to an outer layer.
--
-- >>> let e = extractsExcept [1,2] a
-- >>> shapeList <$> extracts [0] a
-- [[3,4], [3,4]]
extractsExcept ::
  [Int] ->
  Array a ->
  Array (Array a)
extractsExcept ds a = extracts (exclude (rank (shapeList a)) ds) a

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
    so = shapeList a
    si = shapeList (index a (replicate (rank so) 0))

-- | Maps a function along specified dimensions.
--
-- >>> shapeList $ maps (transpose) [1] a
-- [4,3,2]
maps ::
  (Array a -> Array b) ->
  [Int] ->
  Array a ->
  Array b
maps f ds a = joins ds (fmap f (extracts ds a))

-- | Concatenate along a dimension.
--
-- >>> shapeList $ concatenate 1 a a
-- [2,6,4]
concatenate ::
  Int ->
  Array a ->
  Array a ->
  Array a
concatenate d a0 a1 = tabulate (concatenate' d (shapeList a0) (shapeList a1)) go
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
    ds0 = shapeList a0

-- | Insert along a dimension at a position.
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
insert d i a b = tabulate (incAt d (shapeList a)) go
  where
    go s
      | s !! d == i = index b (dropIndex s d)
      | s !! d < i = index a s
      | otherwise = index a (decAt d s)

-- | Insert along a dimension at the end.
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
append d a b = insert d (dimension (shapeList a) d) a b

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
reorder ds a = tabulate (reorder' (shapeList a) ds) go
  where
    go s = index a (addIndexes [] ds s)

-- | reverses order along specified dimensions.
--
-- >>> reverses [0] a
--
reverses ::
  [Int] ->
  Array a ->
  Array a
reverses ds a = tabulate (shapeList a) (index a . reverseIndex ds (shapeList a))

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
-- >>> expand (*) v v
-- [[1, 2, 3],
--  [2, 4, 6],
--  [3, 6, 9]]
--
-- Alternatively, expand can be understood as representing the permutation of element pairs of two arrays, so like the Applicative List instance.
--
-- >>> i2 = indices [2,2]
-- >>> expand (,) i2 i2
-- [[[[([0,0],[0,0]), ([0,0],[0,1])],
--    [([0,0],[1,0]), ([0,0],[1,1])]],
--   [[([0,1],[0,0]), ([0,1],[0,1])],
--    [([0,1],[1,0]), ([0,1],[1,1])]]],
--  [[[([1,0],[0,0]), ([1,0],[0,1])],
--    [([1,0],[1,0]), ([1,0],[1,1])]],
--   [[([1,1],[0,0]), ([1,1],[0,1])],
--    [([1,1],[1,0]), ([1,1],[1,1])]]]]
expand ::
  (a -> b -> c) ->
  Array a ->
  Array b ->
  Array c
expand f a b = tabulate ((++) (shapeList a) (shapeList b)) (\i -> f (index a (List.take r i)) (index b (List.drop r i)))
  where
    r = rank (shapeList a)

-- | Like expand, but permutes the first array first, rather than the second.
--
-- >>> expand (,) v (fmap (+3) v)
-- [[(1,4), (1,5), (1,6)],
--  [(2,4), (2,5), (2,6)],
--  [(3,4), (3,5), (3,6)]]
--
-- >>> expandr (,) v (fmap (+3) v)
-- [[(1,4), (2,4), (3,4)],
--  [(1,5), (2,5), (3,5)],
--  [(1,6), (2,6), (3,6)]]
expandr ::
  (a -> b -> c) ->
  Array a ->
  Array b ->
  Array c
expandr f a b = tabulate ((++) (shapeList a) (shapeList b)) (\i -> f (index a (List.drop r i)) (index b (List.take r i)))
  where
    r = rank (shapeList a)

-- | Apply an array of functions to each array of values.
--
-- This is in the spirit of the applicative functor operation (\<*\>).
--
-- > expand f a b == apply (fmap f a) b
--
-- >>> apply ((*) <$> v) v
-- [[1, 2, 3],
--  [2, 4, 6],
--  [3, 6, 9]]
--
-- Dynamic arrays can't be Applicatives because there is no 'pure' (Shape is not known at compile-time).
--
-- >>> let b = fromFlatList [2,3] [1..6] :: Array Int
-- >>> contract sum [1,2] (apply (fmap (*) b) (transpose b))
-- [[14, 32],
--  [32, 77]]
apply ::
  Array (a -> b) ->
  Array a ->
  Array b
apply f a = tabulate ((++) (shapeList f) (shapeList a)) (\i -> index f (List.take r i) (index a (List.drop r i)))
  where
    r = rank (shapeList f)

-- | Contract an array by applying the supplied (folding) function on diagonal elements of the dimensions.
--
-- This generalises a tensor contraction by allowing the number of contracting diagonals to be other than 2, and allowing a binary operator other than multiplication.
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

-- | A generalisation of a dot operation, which is a multiplicative expansion of two arrays and sum contraction along the middle two dimensions.
--
-- matrix multiplication
--
-- >>> let b = fromFlatList [2,3] [1..6] :: Array Int
-- >>> dot sum (*) b (transpose b)
-- [[14, 32],
--  [32, 77]]
--
-- inner product
--
-- >>> let v = fromFlatList [3] [1..3] :: Array Int
-- >>> dot sum (*) v v
-- 14
--
-- matrix-vector multiplication
-- Note that an `Array Int` with shapeList [3] is neither a row vector nor column vector. `dot` is not turning the vector into a matrix and then using matrix multiplication.
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
    sa = shapeList a

-- | Array multiplication.
--
-- matrix multiplication
--
-- >>> let b = fromFlatList [2,3] [1..6] :: Array Int
-- >>> mult b (transpose b)
-- [[14, 32],
--  [32, 77]]
--
-- inner product
--
-- >>> let v = fromFlatList [3] [1..3] :: Array Int
-- >>> mult v v
-- 14
--
-- matrix-vector multiplication
--
-- >>> mult v b
-- [9, 12, 15]
--
-- >>> mult b v
-- [14, 32]
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
-- >>> s
-- [[[2, 3],
--   [10, 11]],
--  [[14, 15],
--   [22, 23]]]
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
-- >>> shapeList $ squeeze a'
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

-- | Extract specialised to a matrix.
--
-- >>> row 1 m
-- [4, 5, 6, 7]
row :: Int -> Array a -> Array a
row i (Array s a) = Array [n] $ V.slice (i * n) n a
  where
    (_ : n : _) = s

-- | extract specialised to a matrix
--
-- >>> col 1 m
-- [1, 5, 9]
col :: Int -> Array a -> Array a
col i (Array s a) = Array [m] $ V.generate m (\x -> V.unsafeIndex a (i + x * n))
  where
    (m : n : _) = s

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
mmult (Array sx x) (Array sy y) = tabulate [m, n] go
  where
    go [] = throw (NumHaskException "Needs two dimensions")
    go [_] = throw (NumHaskException "Needs two dimensions")
    go (i : j : _) = sum $ V.zipWith (*) (V.slice (fromIntegral i * k) k x) (V.generate k (\x' -> y V.! (fromIntegral j + x' * n)))
    (m : k : _) = sx
    (_ : n : _) = sy
{-# INLINE mmult #-}

-- |
-- >>> D.transpose (fromFlatList [2,2,2] [1..8])
--
-- FIXME: huihua example transposes 001 to 010. A 1 rotation.
-- This transposes 001 to 100
-- | Reverse indices eg transposes the element A/ijk/ to A/kji/.
--
-- >>> index (transpose a) [1,0,0] == index a [0,0,1]
-- True
transpose :: Array a -> Array a
transpose a = tabulate (List.reverse $ shapeList a) (index a . List.reverse)

order :: (Ord a) => Array a -> Array Int
order (Array s v) = Array s (orderV v)

orderBy :: (Ord b) => (a -> b) -> Array a -> Array Int
orderBy c (Array s v) = Array s (orderByV c v)
