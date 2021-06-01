{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | Arrays with a fixed shape.
module NumHask.Array.Fixed
  ( -- $setup
    Array (..),

    -- * Conversion
    with,
    shape,
    toDynamic,

    -- * Operators
    reshape,
    transpose,
    diag,
    ident,
    singleton,
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
    apply,
    contract,
    dot,
    mult,
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

import Data.Distributive (Distributive (..))
import Data.Functor.Rep
import Data.List ((!!))
import qualified Data.Vector as V
import GHC.Exts (IsList (..))
import GHC.Show (Show (..))
import GHC.TypeLits
import qualified NumHask.Array.Dynamic as D
import NumHask.Array.Shape
import NumHask.Prelude as P hiding (toList)
import Data.Proxy

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
-- *** Exception: NumHaskException {errorMessage = "shape mismatch"}
newtype Array s a = Array {unArray :: V.Vector a} deriving (Eq, Ord, Functor, Foldable, Generic, Traversable)

instance (HasShape s, Show a) => Show (Array s a) where
  show a = GHC.Show.show (toDynamic a)

instance
  ( HasShape s
  ) =>
  Data.Distributive.Distributive (Array s)
  where
  distribute = distributeRep
  {-# INLINE distribute #-}

instance
  forall s.
  ( HasShape s
  ) =>
  Representable (Array s)
  where
  type Rep (Array s) = [Int]

  tabulate f =
    Array . V.generate (size s) $ (f . shapen s)
    where
      s = shapeVal $ toShape @s
  {-# INLINE tabulate #-}

  index (Array v) i = V.unsafeIndex v (flatten s i)
    where
      s = shapeVal (toShape @s)
  {-# INLINE index #-}

-- * NumHask heirarchy

instance
  ( Additive a,
    HasShape s
  ) =>
  Additive (Array s a)
  where
  (+) = liftR2 (+)

  zero = pureRep zero

instance
  ( Subtractive a,
    HasShape s
  ) =>
  Subtractive (Array s a)
  where
  negate = fmapRep negate

instance
  (HasShape s, Multiplicative a) =>
  MultiplicativeAction (Array s a) a
  where
  (.*) s r = fmap (s *) r
  {-# INLINE (.*) #-}

  (*.) r s = fmap (* s) r
  {-# INLINE (*.) #-}

instance
  (HasShape s, Additive a) =>
  AdditiveAction (Array s a) a
  where
  (.+) s r = fmap (s +) r
  {-# INLINE (.+) #-}

  (+.) r s = fmap (+ s) r
  {-# INLINE (+.) #-}

instance
  (HasShape s, Subtractive a) =>
  SubtractiveAction (Array s a) a
  where
  (.-) s r = fmap (s -) r
  {-# INLINE (.-) #-}

  (-.) r s = fmap (\x -> x - s) r
  {-# INLINE (-.) #-}

instance
  (HasShape s, Divisive a) =>
  DivisiveAction (Array s a) a
  where
  (./) s r = fmap (s /) r
  {-# INLINE (./) #-}

  (/.) r s = fmap (/ s) r
  {-# INLINE (/.) #-}

instance (HasShape s, JoinSemiLattice a) => JoinSemiLattice (Array s a) where
  (\/) = liftR2 (\/)

instance (HasShape s, MeetSemiLattice a) => MeetSemiLattice (Array s a) where
  (/\) = liftR2 (/\)

instance (HasShape s, Subtractive a, Epsilon a) => Epsilon (Array s a) where
  epsilon = singleton epsilon

  nearZero (Array a) = all nearZero a

instance
  ( HasShape s
  ) =>
  IsList (Array s a)
  where
  type Item (Array s a) = a

  fromList l =
    bool
      (throw (NumHaskException "shape mismatch"))
      (Array $ V.fromList l)
      ((length l == 1 && null ds) || (length l == size ds))
    where
      ds = shapeVal (toShape @s)

  toList (Array v) = V.toList v

-- | Get shape of an Array as a value.
--
-- >>> shape a
-- [2,3,4]
shape :: forall a s. (HasShape s) => Array s a -> [Int]
shape _ = shapeVal $ toShape @s
{-# INLINE shape #-}

-- | convert to a dynamic array with shape at the value level.
toDynamic :: (HasShape s) => Array s a -> D.Array a
toDynamic a = D.fromFlatList (shape a) (toList a)

-- | Use a dynamic array in a fixed context.
--
-- >>> with (D.fromFlatList [2,3,4] [1..24]) (selects (Proxy :: Proxy '[0,1]) [1,1] :: Array '[2,3,4] Int -> Array '[4] Int)
-- [17, 18, 19, 20]
with ::
  forall a r s.
  (HasShape s) =>
  D.Array a ->
  (Array s a -> r) ->
  r
with (D.Array _ v) f = f (Array v)

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
    HasShape s'
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
transpose :: forall a s. (HasShape s, HasShape (Reverse s)) => Array s a -> Array (Reverse s) a
transpose a = tabulate (index a . reverse)

-- | The identity array.
--
-- >>> ident :: Array '[3,2] Int
-- [[1, 0],
--  [0, 1],
--  [0, 0]]
ident :: forall a s. (HasShape s, Additive a, Multiplicative a) => Array s a
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
    HasShape '[Minimum s]
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
singleton :: (HasShape s) => a -> Array s a
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
    s' ~ DropIndexes s ds
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
    s' ~ TakeIndexes s ds
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
    so ~ TakeIndexes st ds
  ) =>
  (Array si a -> b) ->
  Proxy ds ->
  Array st a ->
  Array so b
folds f d a = tabulate go
  where
    go s = f (selects d s a)

-- | Extracts dimensions to an outer layer.
--
-- >>> let e = extracts (Proxy :: Proxy '[1,2]) a
-- >>> :t e
-- e :: Array '[3, 4] (Array '[2] Int)
extracts ::
  forall ds st si so a.
  ( HasShape st,
    HasShape ds,
    HasShape si,
    HasShape so,
    si ~ DropIndexes st ds,
    so ~ TakeIndexes st ds
  ) =>
  Proxy ds ->
  Array st a ->
  Array so (Array si a)
extracts d a = tabulate go
  where
    go s = selects d s a

-- | Extracts /except/ dimensions to an outer layer.
--
-- >>> let e = extractsExcept (Proxy :: Proxy '[1,2]) a
-- >>> :t e
-- e :: Array '[2] (Array '[3, 4] Int)
extractsExcept ::
  forall ds st si so a.
  ( HasShape st,
    HasShape ds,
    HasShape si,
    HasShape so,
    so ~ DropIndexes st ds,
    si ~ TakeIndexes st ds
  ) =>
  Proxy ds ->
  Array st a ->
  Array so (Array si a)
extractsExcept d a = tabulate go
  where
    go s = selectsExcept d s a

-- | Join inner and outer dimension layers.
--
-- >>> let e = extracts (Proxy :: Proxy '[1,0]) a
--
-- >>> :t e
-- e :: Array '[3, 2] (Array '[4] Int)
--
-- >>> let j = joins (Proxy :: Proxy '[1,0]) e
--
-- >>> :t j
-- j :: Array '[2, 3, 4] Int
--
-- >>> a == j
-- True
joins ::
  forall ds si st so a.
  ( HasShape st,
    HasShape ds,
    st ~ AddIndexes si ds so,
    HasShape si,
    HasShape so
  ) =>
  Proxy ds ->
  Array so (Array si a) ->
  Array st a
joins _ a = tabulate go
  where
    go s = index (index a (takeIndexes s ds)) (dropIndexes s ds)
    ds = shapeVal (toShape @ds)

-- | Maps a function along specified dimensions.
--
-- >>> :t maps (transpose) (Proxy :: Proxy '[1]) a
-- maps (transpose) (Proxy :: Proxy '[1]) a :: Array '[4, 3, 2] Int
maps ::
  forall ds st st' si si' so a b.
  ( HasShape st,
    HasShape st',
    HasShape ds,
    HasShape si,
    HasShape si',
    HasShape so,
    si ~ DropIndexes st ds,
    so ~ TakeIndexes st ds,
    st' ~ AddIndexes si' ds so,
    st ~ AddIndexes si ds so
  ) =>
  (Array si a -> Array si' b) ->
  Proxy ds ->
  Array st a ->
  Array st' b
maps f d a = joins d (fmapRep f (extracts d a))

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
    KnownNat d
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
    HasShape (Insert d s)
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
    HasShape (Insert d s)
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
    CheckReorder ds s
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
    HasShape ((++) s s')
  ) =>
  (a -> b -> c) ->
  Array s a ->
  Array s' b ->
  Array ((++) s s') c
expand f a b = tabulate (\i -> f (index a (take r i)) (index b (drop r i)))
  where
    r = rank (shape a)

-- | Apply an array of functions to each array of values.
--
-- This is in the spirit of the applicative functor operation (<*>).
--
-- > expand f a b == apply (fmap f a) b
--
-- >>> apply ((*) <$> v) v
-- [[1, 2, 3],
--  [2, 4, 6],
--  [3, 6, 9]]
--
-- Arrays can't be applicative functors in haskell because the changes in shape are reflected in the types.
--
-- > :t apply
-- apply
--   :: (HasShape s, HasShape s', HasShape (s ++ s')) =>
--      Array s (a -> b) -> Array s' a -> Array (s ++ s') b
-- > :t (<*>)
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
--
-- >>> let b = [1..6] :: Array '[2,3] Int
-- >>> contract sum (Proxy :: Proxy '[1,2]) (apply (fmap (*) b) (transpose b))
-- [[14, 32],
--  [32, 77]]
--
apply ::
  forall s s' a b.
  ( HasShape s,
    HasShape s',
    HasShape ((++) s s')
  ) =>
  Array s (a -> b) ->
  Array s' a ->
  Array ((++) s s') b
apply f a = tabulate (\i -> index f (take r i) (index a (drop r i)))
  where
    r = rank (shape f)

-- | Contract an array by applying the supplied (folding) function on diagonal elements of the dimensions.
--
-- This generalises a tensor contraction by allowing the number of contracting diagonals to be other than 2, and allowing a binary operator other than multiplication.
--
-- >>> let b = [1..6] :: Array '[2,3] Int
-- >>> contract sum (Proxy :: Proxy '[1,2]) (expand (*) b (transpose b))
-- [[14, 32],
--  [32, 77]]
contract ::
  forall a b s ss s' ds.
  ( KnownNat (Minimum (TakeIndexes s ds)),
    HasShape (TakeIndexes s ds),
    HasShape s,
    HasShape ds,
    HasShape ss,
    HasShape s',
    s' ~ DropIndexes s ds,
    ss ~ '[Minimum (TakeIndexes s ds)]
  ) =>
  (Array ss a -> b) ->
  Proxy ds ->
  Array s a ->
  Array s' b
contract f xs a = f . diag <$> extractsExcept xs a

-- | A generalisation of a dot operation, which is a multiplicative expansion of two arrays and sum contraction along the middle two dimensions.
--
-- matrix multiplication
--
-- >>> let b = [1..6] :: Array '[2,3] Int
-- >>> dot sum (*) b (transpose b)
-- [[14, 32],
--  [32, 77]]
--
-- inner product
--
-- >>> let v = [1..3] :: Array '[3] Int
-- >>> :t dot sum (*) v v
-- dot sum (*) v v :: Array '[] Int
--
-- >>> dot sum (*) v v
-- 14
--
-- matrix-vector multiplication
-- (Note how the vector doesn't need to be converted to a row or column vector)
--
-- >>> dot sum (*) v b
-- [9, 12, 15]
--
-- >>> dot sum (*) b v
-- [14, 32]
--
-- dot allows operation on mis-shaped matrices:
--
-- >>> let m23 = [1..6] :: Array '[2,3] Int
-- >>> let m12 = [1,2] :: Array '[1,2] Int
-- >>> shape $ dot sum (*) m23 m12
-- [2,2]
--
-- the algorithm ignores excess positions within the contracting dimension(s):
--
-- m23 shape: 2 3
--
-- m12 shape: 1 2
--
-- res shape: 2 2
--
-- FIXME: work out whether this is a feature or a bug...
--
-- find instances of a vector in a matrix
--
-- >>> let cs = fromList ("abacbaab" :: [Char]) :: Array '[4,2] Char
-- >>> let v = fromList ("ab" :: [Char]) :: Vector 2 Char
-- >>> dot (all id) (==) cs v
-- [True, False, False, True]
dot ::
  forall a b c d sa sb s' ss se.
  ( HasShape sa,
    HasShape sb,
    HasShape (sa ++ sb),
    se ~ TakeIndexes (sa ++ sb) '[Rank sa - 1, Rank sa],
    HasShape se,
    KnownNat (Minimum se),
    KnownNat (Rank sa - 1),
    KnownNat (Rank sa),
    ss ~ '[Minimum se],
    HasShape ss,
    s' ~ DropIndexes (sa ++ sb) '[Rank sa - 1, Rank sa],
    HasShape s'
  ) =>
  (Array ss c -> d) ->
  (a -> b -> c) ->
  Array sa a ->
  Array sb b ->
  Array s' d
dot f g a b = contract f (Proxy :: Proxy '[Rank sa - 1, Rank sa]) (expand g a b)

-- | Array multiplication.
--
-- matrix multiplication
--
-- >>> let b = [1..6] :: Array '[2,3] Int
-- >>> mult b (transpose b)
-- [[14, 32],
--  [32, 77]]
--
-- inner product
--
-- >>> let v = [1..3] :: Array '[3] Int
-- >>> :t mult v v
-- mult v v :: Array '[] Int
--
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
  forall a sa sb s' ss se.
  ( Additive a,
    Multiplicative a,
    HasShape sa,
    HasShape sb,
    HasShape (sa ++ sb),
    se ~ TakeIndexes (sa ++ sb) '[Rank sa - 1, Rank sa],
    HasShape se,
    KnownNat (Minimum se),
    KnownNat (Rank sa - 1),
    KnownNat (Rank sa),
    ss ~ '[Minimum se],
    HasShape ss,
    s' ~ DropIndexes (sa ++ sb) '[Rank sa - 1, Rank sa],
    HasShape s'
  ) =>
  Array sa a ->
  Array sb a ->
  Array s' a
mult = dot sum (*)

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
    s' ~ Ranks pss
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
fromScalar :: (HasShape ('[] :: [Nat])) => Array ('[] :: [Nat]) a -> a
fromScalar a = index a ([] :: [Int])

-- | Convert a number to a scalar.
--
-- >>> :t toScalar 2
-- toScalar 2 :: Num a => Array '[] a
toScalar :: (HasShape ('[] :: [Nat])) => a -> Array ('[] :: [Nat]) a
toScalar a = fromList [a]

-- | <https://en.wikipedia.org/wiki/Vector_(mathematics_and_physics) Wiki Vector>
type Vector s a = Array '[s] a

-- | <https://en.wikipedia.org/wiki/Matrix_(mathematics) Wiki Matrix>
type Matrix m n a = Array '[m, n] a

instance
  ( Multiplicative a,
    P.Distributive a,
    Subtractive a,
    KnownNat m,
    HasShape '[m, m]
  ) =>
  Multiplicative (Matrix m m a)
  where
  (*) = mmult

  one = ident

-- | Extract specialised to a matrix.
--
-- >>> row 1 m
-- [4, 5, 6, 7]
row :: forall m n a. (KnownNat m, KnownNat n, HasShape '[m, n]) => Int -> Matrix m n a -> Vector n a
row i (Array a) = Array $ V.slice (i * n) n a
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
safeRow :: forall m n a j. ('True ~ CheckIndex j m, KnownNat j, KnownNat m, KnownNat n, HasShape '[m, n]) => Proxy j -> Matrix m n a -> Vector n a
safeRow _j (Array a) = Array $ V.slice (j * n) n a
  where
    n = fromIntegral $ natVal @n Proxy
    j = fromIntegral $ natVal @j Proxy

-- | Extract specialised to a matrix.
--
-- >>> col 1 m
-- [1, 5, 9]
col :: forall m n a. (KnownNat m, KnownNat n, HasShape '[m, n]) => Int -> Matrix m n a -> Vector n a
col i (Array a) = Array $ V.generate m (\x -> V.unsafeIndex a (i + x * n))
  where
    m = fromIntegral $ natVal @m Proxy
    n = fromIntegral $ natVal @n Proxy

-- | Column extraction checked at type level.
--
-- >>> safeCol (Proxy :: Proxy 1) m
-- [1, 5, 9]
--
-- >>> safeCol (Proxy :: Proxy 4) m
-- ...
-- ... index outside range
-- ...
safeCol :: forall m n a j. ('True ~ CheckIndex j n, KnownNat j, KnownNat m, KnownNat n, HasShape '[m, n]) => Proxy j -> Matrix m n a -> Vector n a
safeCol _j (Array a) = Array $ V.generate m (\x -> V.unsafeIndex a (j + x * n))
  where
    m = fromIntegral $ natVal @m Proxy
    n = fromIntegral $ natVal @n Proxy
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
    Ring a
  ) =>
  Array [m, k] a ->
  Array [k, n] a ->
  Array [m, n] a
mmult (Array x) (Array y) = tabulate go
  where
    go [] = throw (NumHaskException "Needs two dimensions")
    go [_] = throw (NumHaskException "Needs two dimensions")
    go (i : j : _) = sum $ V.zipWith (*) (V.slice (fromIntegral i * k) k x) (V.generate k (\x' -> y V.! (fromIntegral j + x' * n)))
    n = fromIntegral $ natVal @n Proxy
    k = fromIntegral $ natVal @k Proxy
{-# INLINE mmult #-}

