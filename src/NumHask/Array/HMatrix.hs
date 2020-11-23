{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

-- | Arrays with a fixed shape, with a HMatrix representation.
module NumHask.Array.HMatrix
  ( -- $setup
    Array (..),

    -- * Representation
    --
    -- With no functor instance, we instead supply the representable API
    index,
    tabulate,

    -- * Conversion
    shape,
    toDynamic,
    toFixed,
    fromFixed,

    -- * Operators
    reshape,
    transpose,
    diag,
    ident,
    singleton,
    selects,
    selectsExcept,
    folds,
    concatenate,
    insert,
    append,
    reorder,
    expand,
    slice,
    squeeze,

    -- * Scalar
    --
    -- Scalar specialisations
    Scalar,
    fromScalar,
    toScalar,

    -- * Vector
    --
    -- Vector specialisations.
    Vector,

    -- * Matrix
    --
    -- Matrix specialisations.
    Matrix,
    col,
    row,
    safeCol,
    safeRow,
    mmult,
  )
where

import Data.List ((!!))
import qualified Data.Vector as V
import GHC.Exts (IsList (..))
import GHC.TypeLits
import qualified NumHask.Array.Dynamic as D
import qualified NumHask.Array.Fixed as F
import NumHask.Array.Shape
import NumHask.Prelude as P hiding (transpose)
import qualified Numeric.LinearAlgebra as H
import qualified Numeric.LinearAlgebra.Devel as H
import qualified Prelude

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XOverloadedLists
-- >>> :set -XTypeFamilies
-- >>> :set -XFlexibleContexts
-- >>> let s = [1] :: Array ('[] :: [Nat]) Int -- scalar
-- >>> let v = [1,2,3] :: Array '[3] Int       -- vector
-- >>> let m = [0..11] :: Array '[3,4] Int     -- matrix
-- >>> let a = [1..24] :: Array '[2,3,4] Int

-- | a multidimensional array with a type-level shape
--
-- >>> let a = [1..24] :: Array '[2,3,4] Int
-- >>> a
-- [[[1, 2, 3, 4],
--   [5, 6, 7, 8],
--   [9, 10, 11, 12]],
--  [[13, 14, 15, 16],
--   [17, 18, 19, 20],
--   [21, 22, 23, 24]]]
--
-- >>> [1,2,3] :: Array '[2,2] Int
-- [[*** Exception: NumHaskException {errorMessage = "shape mismatch"}
newtype Array s a = Array {unArray :: H.Matrix a}
  deriving (Show, NFData, Generic)

instance
  ( Additive a,
    HasShape s,
    H.Container H.Vector a,
    Num a
  ) =>
  Additive (Array s a)
  where
  (+) (Array x1) (Array x2) = Array $ H.add x1 x2

  zero = Array $ H.konst zero (n, m)
    where
      s = shapeVal (toShape @s)
      [n, m] = s

instance
  ( Multiplicative a,
    HasShape s,
    H.Container H.Vector a,
    Num (H.Vector a),
    Num a
  ) =>
  Multiplicative (Array s a)
  where
  (*) (Array x1) (Array x2) = Array $ H.liftMatrix2 (Prelude.*) x1 x2

  one = Array $ H.konst one (n, m)
    where
      s = shapeVal (toShape @s)
      [n, m] = s

-- (<.>) (Array a) (Array b) = H.sumElements $ H.liftMatrix2 (Prelude.*) a b

instance
  ( Subtractive a,
    HasShape s,
    H.Container H.Vector a,
    Num a
  ) =>
  Subtractive (Array s a)
  where
  negate (Array x1) = Array $ H.cmap negate x1

instance
  (HasShape s, Multiplicative a, H.Container H.Vector a, Num a) =>
  MultiplicativeAction (Array s a) a
  where
  (.*) s (Array r) = Array $ H.cmap (s *) r
  {-# INLINE (.*) #-}

  (*.) (Array r) s = Array $ H.cmap (*s) r
  {-# INLINE (*.) #-}

instance
  (HasShape s, Additive a, H.Container H.Vector a, Num a) =>
  AdditiveAction (Array s a) a
  where
  (.+) s (Array r) = Array $ H.cmap (s +) r
  {-# INLINE (.+) #-}

  (+.) (Array r) s = Array $ H.cmap (+s) r
  {-# INLINE (+.) #-}

instance
  (HasShape s, Subtractive a, H.Container H.Vector a, Num a) =>
  SubtractiveAction (Array s a) a
  where
  (.-) s (Array r) = Array $ H.cmap (s -) r
  {-# INLINE (.-) #-}

  (-.) (Array r) s = Array $ H.cmap (\x -> x - s) r
  {-# INLINE (-.) #-}

instance
  (HasShape s, Divisive a, H.Container H.Vector a, Num a) =>
  DivisiveAction (Array s a) a
  where
  (./) s (Array r) = Array $ H.cmap (s /) r
  {-# INLINE (./) #-}

  (/.) (Array r) s = Array $ H.cmap (/ s) r
  {-# INLINE (/.) #-}

-- | from flat list
instance
  ( HasShape s,
    H.Element a
  ) =>
  IsList (Array s a)
  where
  type Item (Array s a) = a

  fromList l =
    bool
      (throw (NumHaskException "shape mismatch"))
      (Array $ H.reshape n $ H.fromList l)
      ((length l == 1 && null s) || (length l == size s))
    where
      s = shapeVal (toShape @s)
      n = Prelude.last s

  toList (Array v) = H.toList $ H.flatten v

-- | Get shape of an Array as a value.
--
-- >>> shape a
-- [2,3,4]
shape :: forall a s. (HasShape s) => Array s a -> [Int]
shape _ = shapeVal $ toShape @s
{-# INLINE shape #-}

-- | Convert to a dynamic array.
toDynamic :: (HasShape s, H.Element a) => Array s a -> D.Array a
toDynamic a@(Array h) = D.fromFlatList (shape a) (mconcat $ H.toLists h)

-- | Convert to a fixed array.
toFixed :: (HasShape s, H.Element a) => Array s a -> F.Array s a
toFixed (Array h) = fromList (mconcat $ H.toLists h)

-- | Convert from a fixed array.
fromFixed :: (HasShape s, H.Element a) => F.Array s a -> Array s a
fromFixed a = fromList (P.toList a)

-- | with no fmap, we supply the representable API
index ::
  forall s a.
  ( HasShape s,
    H.Element a,
    H.Container H.Vector a
  ) =>
  Array s a ->
  [Int] ->
  a
index (Array v) i = H.flatten v `H.atIndex` flatten s i
  where
    s = shapeVal (toShape @s)

-- | tabulate an array with a generating function
--
-- >>> tabulate [2,3,4] ((1+) . flatten [2,3,4]) == a
-- True
tabulate ::
  forall s a.
  ( HasShape s,
    H.Element a
  ) =>
  ([Int] -> a) ->
  Array s a
tabulate f =
  fromList (V.toList $ V.generate (size s) (f . shapen s))
  where
    s = shapeVal (toShape @s)

-- | Reshape an array (with the same number of elements).
--
-- >>> reshape a :: Array '[4,3,2] Int
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
  forall a s s'.
  ( Size s ~ Size s',
    HasShape s,
    HasShape s',
    H.Container H.Vector a
  ) =>
  Array s a ->
  Array s' a
reshape a = tabulate (index a . shapen s . flatten s')
  where
    s = shapeVal (toShape @s)
    s' = shapeVal (toShape @s')

-- | Reverse indices eg transposes the element A/ijk/ to A/kji/.
--
-- >>> index (transpose a) [1,0,0] == index a [0,0,1]
-- True
transpose :: forall a s. (H.Element a, H.Container H.Vector a, HasShape s, HasShape (Reverse s)) => Array s a -> Array (Reverse s) a
transpose a = tabulate (index a . reverse)

-- | The identity array.
--
-- >>> ident :: Array '[3,2] Int
-- [[1, 0],
--  [0, 1],
--  [0, 0]]
ident :: forall a s. (H.Element a, H.Container H.Vector a, HasShape s, Additive a, Multiplicative a) => Array s a
ident = tabulate (bool zero one . isDiag)
  where
    isDiag [] = True
    isDiag [_] = True
    isDiag [x, y] = x == y
    isDiag (x : y : xs) = x == y && isDiag (y : xs)

-- | Extract the diagonal of an array.
--
-- >>> diag (ident :: Array '[3,2] Int)
-- [1, 1]
diag ::
  forall a s.
  ( HasShape s,
    HasShape '[Minimum s],
    H.Element a,
    H.Container H.Vector a
  ) =>
  Array s a ->
  Array '[Minimum s] a
diag a = tabulate go
  where
    go [] = throw (NumHaskException "Rank Underflow")
    go (s' : _) = index a (replicate (length ds) s')
    ds = shapeVal (toShape @s)

-- | Create an array composed of a single value.
--
-- >>> singleton one :: Array '[3,2] Int
-- [[1, 1],
--  [1, 1],
--  [1, 1]]
singleton :: (H.Element a, H.Container H.Vector a, HasShape s) => a -> Array s a
singleton a = tabulate (const a)

-- | Select an array along dimensions.
--
-- >>> let s = selects (Proxy :: Proxy '[0,1]) [1,1] a
-- >>> :t s
-- s :: Array '[4] Int
--
-- >>> s
-- [17, 18, 19, 20]
selects ::
  forall ds s s' a.
  ( HasShape s,
    HasShape ds,
    HasShape s',
    s' ~ DropIndexes s ds,
    H.Element a,
    H.Container H.Vector a
  ) =>
  Proxy ds ->
  [Int] ->
  Array s a ->
  Array s' a
selects _ i a = tabulate go
  where
    go s = index a (addIndexes s ds i)
    ds = shapeVal (toShape @ds)

-- | Select an index /except/ along specified dimensions.
--
-- >>> let s = selectsExcept (Proxy :: Proxy '[2]) [1,1] a
-- >>> :t s
-- s :: Array '[4] Int
--
-- >>> s
-- [17, 18, 19, 20]
selectsExcept ::
  forall ds s s' a.
  ( HasShape s,
    HasShape ds,
    HasShape s',
    s' ~ TakeIndexes s ds,
    H.Element a,
    H.Container H.Vector a
  ) =>
  Proxy ds ->
  [Int] ->
  Array s a ->
  Array s' a
selectsExcept _ i a = tabulate go
  where
    go s = index a (addIndexes i ds s)
    ds = shapeVal (toShape @ds)

-- | Fold along specified dimensions.
--
-- >>> folds sum (Proxy :: Proxy '[1]) a
-- [68, 100, 132]
folds ::
  forall ds st si so a b.
  ( HasShape st,
    HasShape ds,
    HasShape si,
    HasShape so,
    si ~ DropIndexes st ds,
    so ~ TakeIndexes st ds,
    H.Element a,
    H.Container H.Vector a,
    H.Element b,
    H.Container H.Vector b
  ) =>
  (Array si a -> b) ->
  Proxy ds ->
  Array st a ->
  Array so b
folds f d a = tabulate go
  where
    go s = f (selects d s a)

-- | Concatenate along a dimension.
--
-- >>> :t concatenate (Proxy :: Proxy 1) a a
-- concatenate (Proxy :: Proxy 1) a a :: Array '[2, 6, 4] Int
concatenate ::
  forall a s0 s1 d s.
  ( CheckConcatenate d s0 s1 s,
    Concatenate d s0 s1 ~ s,
    HasShape s0,
    HasShape s1,
    HasShape s,
    KnownNat d,
    H.Element a,
    H.Container H.Vector a
  ) =>
  Proxy d ->
  Array s0 a ->
  Array s1 a ->
  Array s a
concatenate _ s0 s1 = tabulate go
  where
    go s =
      bool
        (index s0 s)
        ( index
            s1
            ( addIndex
                (dropIndex s d)
                d
                ((s !! d) - (ds0 !! d))
            )
        )
        ((s !! d) >= (ds0 !! d))
    ds0 = shapeVal (toShape @s0)
    d = fromIntegral $ natVal @d Proxy

-- | Insert along a dimension at a position.
--
-- >>> insert (Proxy :: Proxy 2) (Proxy :: Proxy 0) a ([100..105])
-- [[[100, 1, 2, 3, 4],
--   [101, 5, 6, 7, 8],
--   [102, 9, 10, 11, 12]],
--  [[103, 13, 14, 15, 16],
--   [104, 17, 18, 19, 20],
--   [105, 21, 22, 23, 24]]]
insert ::
  forall a s s' d i.
  ( DropIndex s d ~ s',
    CheckInsert d i s,
    KnownNat i,
    KnownNat d,
    HasShape s,
    HasShape s',
    HasShape (Insert d s),
    H.Element a,
    H.Container H.Vector a
  ) =>
  Proxy d ->
  Proxy i ->
  Array s a ->
  Array s' a ->
  Array (Insert d s) a
insert _ _ a b = tabulate go
  where
    go s
      | s !! d == i = index b (dropIndex s d)
      | s !! d < i = index a s
      | otherwise = index a (decAt d s)
    d = fromIntegral $ natVal @d Proxy
    i = fromIntegral $ natVal @i Proxy

-- | Insert along a dimension at the end.
--
-- >>>  :t append (Proxy :: Proxy 0) a
-- append (Proxy :: Proxy 0) a
--   :: Array '[3, 4] Int -> Array '[3, 3, 4] Int
append ::
  forall a d s s'.
  ( DropIndex s d ~ s',
    CheckInsert d (Dimension s d - 1) s,
    KnownNat (Dimension s d - 1),
    KnownNat d,
    HasShape s,
    HasShape s',
    HasShape (Insert d s),
    H.Element a,
    H.Container H.Vector a
  ) =>
  Proxy d ->
  Array s a ->
  Array s' a ->
  Array (Insert d s) a
append d = insert d (Proxy :: Proxy (Dimension s d - 1))

-- | Change the order of dimensions.
--
-- >>> let r = reorder (Proxy :: Proxy '[2,0,1]) a
-- >>> :t r
-- r :: Array '[4, 2, 3] Int
reorder ::
  forall a ds s.
  ( HasShape ds,
    HasShape s,
    HasShape (Reorder s ds),
    CheckReorder ds s,
    H.Element a,
    H.Container H.Vector a
  ) =>
  Proxy ds ->
  Array s a ->
  Array (Reorder s ds) a
reorder _ a = tabulate go
  where
    go s = index a (addIndexes [] ds s)
    ds = shapeVal (toShape @ds)

-- | Product two arrays using the supplied binary function.
--
-- For context, if the function is multiply, and the arrays are tensors,
-- then this can be interpreted as a tensor product.
--
-- https://en.wikipedia.org/wiki/Tensor_product
--
-- The concept of a tensor product is a dense crossroad, and a complete treatment is elsewhere.  To quote:
--
-- ... the tensor product can be extended to other categories of mathematical objects in addition to vector spaces, such as to matrices, tensors, algebras, topological vector spaces, and modules. In each such case the tensor product is characterized by a similar universal property: it is the freest bilinear operation. The general concept of a "tensor product" is captured by monoidal categories; that is, the class of all things that have a tensor product is a monoidal category.
--
-- >>> expand (*) v v
-- [[1, 2, 3],
--  [2, 4, 6],
--  [3, 6, 9]]
expand ::
  forall s s' a b c.
  ( HasShape s,
    HasShape s',
    HasShape ((++) s s'),
    H.Element a,
    H.Container H.Vector a,
    H.Element b,
    H.Container H.Vector b,
    H.Element c
  ) =>
  (a -> b -> c) ->
  Array s a ->
  Array s' b ->
  Array ((++) s s') c
expand f a b = tabulate (\i -> f (index a (take r i)) (index b (drop r i)))
  where
    r = rank (shape a)

-- | Select elements along positions in every dimension.
--
-- >>> let s = slice (Proxy :: Proxy '[[0,1],[0,2],[1,2]]) a
-- >>> :t s
-- s :: Array '[2, 2, 2] Int
--
-- >>> s
-- [[[2, 3],
--   [10, 11]],
--  [[14, 15],
--   [22, 23]]]
--
-- >>> let s = squeeze $ slice (Proxy :: Proxy '[ '[0], '[0], '[0]]) a
-- >>> :t s
-- s :: Array '[] Int
--
-- >>> s
-- 1
slice ::
  forall (pss :: [[Nat]]) s s' a.
  ( HasShape s,
    HasShape s',
    KnownNatss pss,
    KnownNat (Rank pss),
    s' ~ Ranks pss,
    H.Element a,
    H.Container H.Vector a
  ) =>
  Proxy pss ->
  Array s a ->
  Array s' a
slice pss a = tabulate go
  where
    go s = index a (zipWith (!!) pss' s)
    pss' = natValss pss

-- | Remove single dimensions.
--
-- >>> let a = [1..24] :: Array '[2,1,3,4,1] Int
-- >>> a
-- [[[[[1],
--     [2],
--     [3],
--     [4]],
--    [[5],
--     [6],
--     [7],
--     [8]],
--    [[9],
--     [10],
--     [11],
--     [12]]]],
--  [[[[13],
--     [14],
--     [15],
--     [16]],
--    [[17],
--     [18],
--     [19],
--     [20]],
--    [[21],
--     [22],
--     [23],
--     [24]]]]]
-- >>> squeeze a
-- [[[1, 2, 3, 4],
--   [5, 6, 7, 8],
--   [9, 10, 11, 12]],
--  [[13, 14, 15, 16],
--   [17, 18, 19, 20],
--   [21, 22, 23, 24]]]
--
-- >>> squeeze ([1] :: Array '[1,1] Double)
-- 1.0
squeeze ::
  forall s t a.
  (t ~ Squeeze s) =>
  Array s a ->
  Array t a
squeeze (Array x) = Array x

-- $scalar
-- Scalar specialisations

-- | <https://en.wikipedia.org/wiki/Scalarr_(mathematics) Wiki Scalar>
--
-- An Array '[] a despite being a Scalar is never-the-less a one-element vector under the hood. Unification of representation is unexplored.
type Scalar a = Array ('[] :: [Nat]) a

-- | Unwrapping scalars is probably a performance bottleneck.
--
-- >>> let s = [3] :: Array ('[] :: [Nat]) Int
-- >>> fromScalar s
-- 3
fromScalar :: (H.Element a, H.Container H.Vector a, HasShape ('[] :: [Nat])) => Array ('[] :: [Nat]) a -> a
fromScalar a = index a ([] :: [Int])

-- | Convert a number to a scalar.
--
-- >>> :t toScalar 2
-- toScalar 2 :: Num a => Array '[] a
toScalar :: (H.Element a, H.Container H.Vector a, HasShape ('[] :: [Nat])) => a -> Array ('[] :: [Nat]) a
toScalar a = fromList [a]

-- | <https://en.wikipedia.org/wiki/Vector_(mathematics_and_physics) Wiki Vector>
type Vector s a = Array '[s] a

-- | <https://en.wikipedia.org/wiki/Matrix_(mathematics) Wiki Matrix>
type Matrix m n a = Array '[m, n] a

instance
  ( Multiplicative a,
    P.Distributive a,
    Subtractive a,
    H.Numeric a,
    KnownNat m,
    HasShape '[m, m],
    H.Element a,
    H.Container H.Vector a
  ) =>
  Multiplicative (Matrix m m a)
  where
  (*) = mmult

  one = ident

-- | Extract specialised to a matrix.
--
-- >>> row 1 m
-- [4, 5, 6, 7]
row :: forall m n a. (H.Element a, H.Container H.Vector a, KnownNat m, KnownNat n, HasShape '[m, n]) => Int -> Matrix m n a -> Vector n a
row i (Array a) = fromList $ H.toList $ H.subVector (i * n) n (H.flatten a)
  where
    n = fromIntegral $ natVal @n Proxy

-- | Row extraction checked at type level.
--
-- >>> safeRow (Proxy :: Proxy 1) m
-- [4, 5, 6, 7]
--
-- >>> safeRow (Proxy :: Proxy 3) m
-- ...
-- ... index outside range
-- ...
safeRow :: forall m n a j. (H.Element a, H.Container H.Vector a, 'True ~ CheckIndex j m, KnownNat j, KnownNat m, KnownNat n, HasShape '[m, n]) => Proxy j -> Matrix m n a -> Vector n a
safeRow _j (Array a) = fromList $ H.toList $ H.subVector (j * n) n (H.flatten a)
  where
    n = fromIntegral $ natVal @n Proxy
    j = fromIntegral $ natVal @j Proxy

-- | Extract specialised to a matrix.
--
-- >>> col 1 m
-- [1, 5, 9]
col :: forall m n a. (H.Element a, H.Container H.Vector a, KnownNat m, KnownNat n, HasShape '[m, n]) => Int -> Matrix m n a -> Vector n a
col i (Array a) = Array $ H.takeColumns i a

-- | Column extraction checked at type level.
--
-- >>> safeCol (Proxy :: Proxy 1) m
-- [1, 5, 9]
--
-- >>> safeCol (Proxy :: Proxy 4) m
-- ...
-- ... index outside range
-- ...
safeCol :: forall m n a j. (H.Element a, H.Container H.Vector a, 'True ~ CheckIndex j n, KnownNat j, KnownNat m, KnownNat n, HasShape '[m, n]) => Proxy j -> Matrix m n a -> Vector n a
safeCol _j (Array a) = Array $ H.takeColumns j a
  where
    j = fromIntegral $ natVal @j Proxy

-- | Matrix multiplication.
--
-- This is dot sum (*) specialised to matrices
--
-- >>> let a = [1, 2, 3, 4] :: Array '[2, 2] Int
-- >>> let b = [5, 6, 7, 8] :: Array '[2, 2] Int
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
  forall m n k a.
  ( KnownNat k,
    KnownNat m,
    KnownNat n,
    HasShape [m, n],
    Ring a,
    H.Numeric a
  ) =>
  Array [m, k] a ->
  Array [k, n] a ->
  Array [m, n] a
mmult (Array x) (Array y) = Array $ x H.<> y
