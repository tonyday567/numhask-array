{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Arrays with a fixed shape (known shape at compile time).
module NumHask.Array.Fixed
  ( -- $usage
    Array (UnsafeArray, Array, toVector),

    -- * shape
    shape,
    shapeA,
    rank,
    size,

    -- * Conversion
    with,
    toDynamic,
    fromScalar,
    toScalar,

    -- * Operators
    takes,
    reshape,
    transpose,
    indices,
    ident,
    sequent,
    diag,
    undiag,
    singleton,
    selects,
    selectsExcept,
    folds,
    extracts,
    extractsExcept,
    joins,
    maps,
    NumHask.Array.Fixed.concatenate,
    insert,
    append,
    NumHask.Array.Fixed.reorder,
    reverses,
    rotates,
    expand,
    expandr,
    apply,
    contract,
    dot,
    mult,
    slice,
    NumHask.Array.Fixed.squeeze,

    -- * Vector specialisations.
    Vector,
    sequentv,

    -- * Matrix
    Matrix,
    col,
    row,
    safeCol,
    safeRow,
    mmult,
    chol,
    invtri,
  )
where

import Data.Distributive (Distributive (..))
import Data.Functor.Classes
import Data.Functor.Rep
import Data.Proxy
import Data.Vector qualified as V
import GHC.Exts (IsList (..))
import GHC.TypeLits
import NumHask.Array.Dynamic qualified as D
import NumHask.Array.Shape hiding (rank, size)
import NumHask.Array.Shape qualified as S
import NumHask.Prelude as P hiding (sequence, toList)
import Prettyprinter hiding (dot)

-- $setup
--
-- >>> :m -Prelude
-- >>> :set -XDataKinds
-- >>> :set -XOverloadedLists
-- >>> :set -XTypeFamilies
-- >>> :set -XFlexibleContexts
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude
-- >>> import GHC.TypeLits (Nat)
-- >>> import Data.Proxy
-- >>> import Prettyprinter hiding (dot)
-- >>> import Data.Functor.Rep
-- >>> let s = [1] :: Array ('[]::[Nat]) Int -- scalar
-- >>> let v = [1,2,3] :: Array '[3] Int       -- vector
-- >>> let t = [0..3] :: Array '[2,2] Int     -- square matrix
-- >>> let m = [0..11] :: Array '[3,4] Int     -- matrix
-- >>> let a = [1..24] :: Array '[2,3,4] Int

-- $usage
--
-- >>> :set -XDataKinds
-- >>> :set -XOverloadedLists
-- >>> :set -XTypeFamilies
-- >>> :set -XFlexibleContexts
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude
-- >>> import NumHask.Array.Fixed
-- >>> import GHC.TypeLits (Nat)
-- >>> let s = [1] :: Array ('[]::[Nat]) Int -- scalar
-- >>> let v = [1,2,3] :: Array '[3] Int       -- vector
-- >>> let m = [0..11] :: Array '[3,4] Int     -- matrix
-- >>> let a = [1..24] :: Array '[2,3,4] Int

-- | a multidimensional array with a type-level shape
--
-- >>> :set -XDataKinds
-- >>> [1..24] :: Array '[2,3,4] Int
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24]
-- >>> pretty ([1..24] :: Array '[2,3,4] Int)
-- [[[1,2,3,4],
--   [5,6,7,8],
--   [9,10,11,12]],
--  [[13,14,15,16],
--   [17,18,19,20],
--   [21,22,23,24]]]
--
-- >>> [1,2,3] :: Array '[2,2] Int
-- *** Exception: NumHaskException {errorMessage = "shape mismatch"}
type role Array nominal representational

newtype Array (s :: [Nat]) a where
  UnsafeArray :: V.Vector a -> Array s a
  deriving stock (Functor, Foldable, Generic, Traversable)
  deriving newtype (Eq, Eq1, Ord, Ord1, Show, Show1)

pattern Array :: V.Vector a -> Array s a
pattern Array {toVector} <- UnsafeArray toVector

{-# COMPLETE Array #-}

instance (HasShape s, Show a) => Pretty (Array s a) where
  pretty = pretty . toDynamic

instance
  (HasShape s) =>
  Data.Distributive.Distributive (Array s)
  where
  distribute = distributeRep
  {-# INLINE distribute #-}

instance
  forall s.
  (HasShape s) =>
  Representable (Array s)
  where
  type Rep (Array s) = [Int]

  tabulate f =
    UnsafeArray . V.generate (S.size s) $ (f . shapen s)
    where
      s = shapeOf @s
  {-# INLINE tabulate #-}

  index (Array v) i = V.unsafeIndex v (flatten s i)
    where
      s = shapeOf @s
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
  (Multiplicative a) =>
  MultiplicativeAction (Array s a)
  where
  type Scalar (Array s a) = a
  (|*) r s = fmap (s *) r

instance (Additive a) => AdditiveAction (Array s a) where
  type AdditiveScalar (Array s a) = a
  (|+) r s = fmap (s +) r

instance
  (Subtractive a) =>
  SubtractiveAction (Array s a)
  where
  (|-) r s = fmap (\x -> x - s) r

instance
  (Divisive a) =>
  DivisiveAction (Array s a)
  where
  (|/) r s = fmap (/ s) r

instance (HasShape s, JoinSemiLattice a) => JoinSemiLattice (Array s a) where
  (\/) = liftR2 (\/)

instance (HasShape s, MeetSemiLattice a) => MeetSemiLattice (Array s a) where
  (/\) = liftR2 (/\)

instance (HasShape s, Subtractive a, Epsilon a) => Epsilon (Array s a) where
  epsilon = singleton epsilon

instance (FromInteger a) => FromInteger (Array ('[] :: [Nat]) a) where
  fromInteger x = toScalar (fromInteger x)

instance (FromRational a) => FromRational (Array ('[] :: [Nat]) a) where
  fromRational x = toScalar (fromRational x)

instance
  (HasShape s) =>
  IsList (Array s a)
  where
  type Item (Array s a) = a

  fromList l =
    bool
      (throw (NumHaskException "shape mismatch"))
      (UnsafeArray $ V.fromList l)
      ((length l == 1 && null ds) || (length l == S.size ds))
    where
      ds = shapeOf @s

  toList (Array v) = V.toList v

-- | Get shape of an Array as a value.
--
-- >>> shape a
-- [2,3,4]
shape :: forall a s. (HasShape s) => Array s a -> [Int]
shape _ = shapeOf @s
{-# INLINE shape #-}

-- | Get shape of an Array as an Array.
--
-- >>> shape a
-- [2,3,4]
shapeA :: forall a s n. (HasShape s, KnownNat n) => Array s a -> Array '[n] Int
shapeA a = fromList (shape a)
{-# INLINE shapeA #-}

-- | Get rank of an Array as a value.
--
-- >>> rank a
-- 3
rank :: forall a s. (HasShape s) => Array s a -> Int
rank = S.rank . shape
{-# INLINE rank #-}

-- | Get size of an Array as a value.
--
-- >>> size a
-- 24
size :: forall a s. (HasShape s) => Array s a -> Int
size = S.size . shape
{-# INLINE size #-}

-- | convert to a dynamic array with shape at the value level.
toDynamic :: (HasShape s) => Array s a -> D.Array a
toDynamic a = D.array (shape a) (toList a)

-- | Use a dynamic array in a fixed context.
--
-- >>> import qualified NumHask.Array.Dynamic as D
-- >>> d = D.array ([2,3,4]::[Int]) ([1..24] :: [Int]) :: D.Array Int
-- >>> pretty $ with d (selects (Proxy :: Proxy '[0,1]) [1,1] :: Array '[2,3,4] Int -> Array '[4] Int)
-- [17,18,19,20]
with ::
  forall a r s.
  (HasShape s) =>
  D.Array a ->
  (Array s a -> r) ->
  r
with (D.UnsafeArray _ v) f = f (UnsafeArray v)

-- | Takes the top-most elements according to the new dimension.
--
-- >>> pretty (takes a :: Array '[2,2,3] Int)
-- [[[1,2,3],
--   [5,6,7]],
--  [[13,14,15],
--   [17,18,19]]]
takes ::
  forall s s' a.
  ( HasShape s,
    HasShape s'
  ) =>
  Array s a ->
  Array s' a
takes a = tabulate $ \s -> index a s

-- | Reshape an array (with the same number of elements).
--
-- >>> pretty (reshape a :: Array '[4,3,2] Int)
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
  forall a s s'.
  ( Size s ~ Size s',
    HasShape s,
    HasShape s'
  ) =>
  Array s a ->
  Array s' a
reshape a = tabulate (index a . shapen s . flatten s')
  where
    s = shapeOf @s
    s' = shapeOf @s'

-- | Reverse indices eg transposes the element A/ijk/ to A/kji/.
--
-- >>> index (transpose a) [1,0,0] == index a [0,0,1]
-- True
transpose :: forall a s. (HasShape s, HasShape (Reverse s)) => Array s a -> Array (Reverse s) a
transpose a = tabulate (index a . reverse)

-- | Indices of an Array.
--
-- >>> pretty (indices :: Array '[3,3] [Int])
-- [[[0,0],[0,1],[0,2]],
--  [[1,0],[1,1],[1,2]],
--  [[2,0],[2,1],[2,2]]]
indices :: forall s. (HasShape s) => Array s [Int]
indices = tabulate id

-- | The identity array.
--
-- >>> pretty (ident :: Array '[3,2] Int)
-- [[1,0],
--  [0,1],
--  [0,0]]
ident :: forall a s. (HasShape s, Additive a, Multiplicative a) => Array s a
ident = tabulate (bool zero one . isDiag)

-- | An array of sequential Ints
--
-- >>> pretty (sequent :: Array '[3] Int)
-- [0,1,2]
--
-- >>> pretty (sequent :: Array '[3,3] Int)
-- [[0,0,0],
--  [0,1,0],
--  [0,0,2]]
sequent :: forall s. (HasShape s) => Array s Int
sequent = tabulate go
  where
    go [] = zero
    go [i] = i
    go (i : js) = bool zero i (all (i ==) js)

-- | Extract the diagonal of an array.
--
-- >>> pretty (diag (ident :: Array '[3,2] Int))
-- [1,1]
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
    ds = shapeOf @s

-- | Expand the array to form a diagonal array
--
-- >>> pretty (undiag ([1,1] :: Array '[2] Int))
-- [[1,0],
--  [0,1]]
undiag ::
  forall a s.
  ( HasShape s,
    Additive a,
    HasShape ((++) s s)
  ) =>
  Array s a ->
  Array ((++) s s) a
undiag a = tabulate go
  where
    go [] = throw (NumHaskException "Rank Underflow")
    go xs@(x : xs') = bool zero (index a xs) (all (x ==) xs')

-- | Create an array composed of a single value.
--
-- >>> pretty (singleton one :: Array '[3,2] Int)
-- [[1,1],
--  [1,1],
--  [1,1]]
singleton :: (HasShape s) => a -> Array s a
singleton a = tabulate (const a)

-- | Select an array along dimensions.
--
-- >>> let s = selects (Proxy :: Proxy '[0,1]) [1,1] a
-- >>> :t s
-- s :: Array '[4] Int
--
-- >>> pretty $ s
-- [17,18,19,20]
selects ::
  forall ds s s' a.
  ( HasShape s,
    HasShape ds,
    HasShape s',
    s' ~ DeleteDims ds s
  ) =>
  Proxy ds ->
  [Int] ->
  Array s a ->
  Array s' a
selects _ i a = tabulate go
  where
    go s = index a (insertDims ds i s)
    ds = shapeOf @ds

-- | Select an index /except/ along specified dimensions.
--
-- >>> let s = selectsExcept (Proxy :: Proxy '[2]) [1,1] a
-- >>> :t s
-- s :: Array '[4] Int
--
-- >>> pretty $ s
-- [17,18,19,20]
selectsExcept ::
  forall ds s s' a.
  ( HasShape s,
    HasShape ds,
    HasShape s',
    s' ~ TakeDims ds s
  ) =>
  Proxy ds ->
  [Int] ->
  Array s a ->
  Array s' a
selectsExcept _ i a = tabulate go
  where
    go s = index a (insertDims ds s i)
    ds = shapeOf @ds

-- | Fold along specified dimensions.
--
-- >>> pretty $ folds sum (Proxy :: Proxy '[1]) a
-- [68,100,132]
folds ::
  forall ds st si so a b.
  ( HasShape st,
    HasShape ds,
    HasShape si,
    HasShape so,
    si ~ DeleteDims ds st,
    so ~ TakeDims ds st
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
-- e :: Array [3, 4] (Array '[2] Int)
extracts ::
  forall ds st si so a.
  ( HasShape st,
    HasShape ds,
    HasShape si,
    HasShape so,
    si ~ DeleteDims ds st,
    so ~ TakeDims ds st
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
-- e :: Array '[2] (Array [3, 4] Int)
extractsExcept ::
  forall ds st si so a.
  ( HasShape st,
    HasShape ds,
    HasShape si,
    HasShape so,
    so ~ DeleteDims ds st,
    si ~ TakeDims ds st
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
-- >>> let j = joins (Proxy :: Proxy '[1,0]) e
-- >>> a == j
-- True
joins ::
  forall ds si st so a.
  ( HasShape st,
    HasShape ds,
    st ~ InsertDims ds so si,
    HasShape si,
    HasShape so
  ) =>
  Proxy ds ->
  Array so (Array si a) ->
  Array st a
joins _ a = tabulate go
  where
    go s = index (index a (takeDims ds s)) (deleteDims ds s)
    ds = shapeOf @ds

-- | Maps a function along specified dimensions.
--
-- >>> :t maps (transpose) (Proxy :: Proxy '[1]) a
-- maps (transpose) (Proxy :: Proxy '[1]) a :: Array [4, 3, 2] Int
maps ::
  forall ds st st' si si' so a b.
  ( HasShape st,
    HasShape st',
    HasShape ds,
    HasShape si,
    HasShape si',
    HasShape so,
    si ~ DeleteDims ds st,
    so ~ TakeDims ds st,
    st' ~ InsertDims ds so si',
    st ~ InsertDims ds so si
  ) =>
  (Array si a -> Array si' b) ->
  Proxy ds ->
  Array st a ->
  Array st' b
maps f d a = joins d (fmapRep f (extracts d a))

-- | Concatenate along a dimension.
--
-- >>> :t concatenate (Proxy :: Proxy 1) a a
-- concatenate (Proxy :: Proxy 1) a a :: Array [2, 6, 4] Int
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
            ( insertDim
                d
                ((s !! d) - (ds0 !! d))
                (deleteDim d s)
            )
        )
        ((s !! d) >= (ds0 !! d))
    ds0 = shapeOf @s0
    d = valueOf @d

-- | Insert along a dimension at a position.
--
-- >>> pretty (insert (Proxy :: Proxy 2) (Proxy :: Proxy 0) a ([100..105]))
-- [[[100,1,2,3,4],
--   [101,5,6,7,8],
--   [102,9,10,11,12]],
--  [[103,13,14,15,16],
--   [104,17,18,19,20],
--   [105,21,22,23,24]]]
insert ::
  forall a s s' d i.
  ( DeleteDim d s ~ s',
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
      | s !! d == i = index b (deleteDim d s)
      | s !! d < i = index a s
      | otherwise = index a (decAt d s)
    d = fromIntegral $ natVal @d Proxy
    i = fromIntegral $ natVal @i Proxy

-- | Append along a dimension at the end.
--
-- >>>  :t append (Proxy :: Proxy 0) a
-- append (Proxy :: Proxy 0) a
--   :: Array [3, 4] Int -> Array [3, 3, 4] Int
append ::
  forall a d s s'.
  ( DeleteDim d s ~ s',
    CheckInsert d (IndexOf d s - 1) s,
    KnownNat (IndexOf d s - 1),
    KnownNat d,
    HasShape s,
    HasShape s',
    HasShape (Insert d s)
  ) =>
  Proxy d ->
  Array s a ->
  Array s' a ->
  Array (Insert d s) a
append d = insert d (Proxy :: Proxy (IndexOf d s - 1))

-- | Change the order of dimensions.
--
-- >>> let r = reorder (Proxy :: Proxy '[2,0,1]) a
-- >>> :t r
-- r :: Array [4, 2, 3] Int
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
    go s = index a (insertDims [] (shapeOf @ds) s)

-- | reverses order along specified dimensions.
--
-- > reverses [0] a
reverses ::
  forall a s.
  (HasShape s) =>
  [Int] ->
  Array s a ->
  Array s a
reverses ds a = tabulate (index a . reverseIndex ds (shapeOf @s))

rotates ::
  forall a s.
  (HasShape s) =>
  [(Int, Int)] ->
  Array s a ->
  Array s a
rotates rs a = tabulate (index a . rotateIndex rs (shapeOf @s))

-- | Product two arrays using the supplied binary function.
--
-- For context, if the function is multiply, and the arrays are tensors,
-- then this can be interpreted as a tensor product.
--
-- < https://en.wikipedia.org/wiki/Tensor_product>
--
-- The concept of a tensor product is a dense crossroad, and a complete treatment is elsewhere.  To quote:
--
-- ... the tensor product can be extended to other categories of mathematical objects in addition to vector spaces, such as to matrices, tensors, algebras, topological vector spaces, and modules. In each such case the tensor product is characterized by a similar universal property: it is the freest bilinear operation. The general concept of a "tensor product" is captured by monoidal categories; that is, the class of all things that have a tensor product is a monoidal category.
--
-- >>> pretty $ expand (*) v v
-- [[1,2,3],
--  [2,4,6],
--  [3,6,9]]
--
-- Alternatively, expand can be understood as representing the permutation of element pairs of two arrays, so like the Applicative List instance.
--
-- >>> i2 = indices :: Array '[2,2] [Int]
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
    r = rank a

-- | Like expand, but permutes the first array first, rather than the second.
--
-- >>> pretty $ expand (,) v (v |+ 3)
-- [[(1,4),(1,5),(1,6)],
--  [(2,4),(2,5),(2,6)],
--  [(3,4),(3,5),(3,6)]]
--
-- >>> pretty $ expandr (,) v (v |+ 3)
-- [[(1,4),(2,4),(3,4)],
--  [(1,5),(2,5),(3,5)],
--  [(1,6),(2,6),(3,6)]]
expandr ::
  forall s s' a b c.
  ( HasShape s,
    HasShape s',
    HasShape ((++) s s')
  ) =>
  (a -> b -> c) ->
  Array s a ->
  Array s' b ->
  Array ((++) s s') c
expandr f a b = tabulate (\i -> f (index a (drop r i)) (index b (take r i)))
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
-- Fixed Arrays can't be applicative functors because the changes in shape are reflected in the types.
--
-- > :t apply
-- > apply
-- >   :: (HasShape s, HasShape s', HasShape (s ++ s')) =>
-- >      Array s (a -> b) -> Array s' a -> Array (s ++ s') b
-- > :t (<*>)
-- > (<*>) :: Applicative f => f (a -> b) -> f a -> f b
--
-- >>> let b = [1..6] :: Array '[2,3] Int
-- >>> pretty $ contract sum (Proxy :: Proxy '[1,2]) (apply (fmap (*) b) (transpose b))
-- [[14,32],
--  [32,77]]
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
    r = rank a

-- | Contract an array by applying the supplied (folding) function on diagonal elements of the dimensions.
--
-- This generalises a tensor contraction by allowing the number of contracting diagonals to be other than 2, and allowing a binary operator other than multiplication.
--
-- >>> let b = [1..6] :: Array '[2,3] Int
-- >>> pretty $ contract sum (Proxy :: Proxy '[1,2]) (expand (*) b (transpose b))
-- [[14,32],
--  [32,77]]
contract ::
  forall a b s ss s' ds.
  ( KnownNat (Minimum (TakeDims ds s)),
    HasShape (TakeDims ds s),
    HasShape s,
    HasShape ds,
    HasShape ss,
    HasShape s',
    s' ~ DeleteDims ds s,
    ss ~ '[Minimum (TakeDims ds s)]
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
-- >>> pretty $ dot sum (*) b (transpose b)
-- [[14,32],
--  [32,77]]
--
-- inner product
--
-- >>> let v = [1..3] :: Array '[3] Int
-- >>> :t dot sum (*) v v
-- dot sum (*) v v :: Array '[] Int
--
-- >>> pretty $ dot sum (*) v v
-- 14
--
-- matrix-vector multiplication
-- (Note how the vector doesn't need to be converted to a row or column vector)
--
-- >>> pretty $ dot sum (*) v b
-- [9,12,15]
--
-- >>> pretty $ dot sum (*) b v
-- [14,32]
--
-- Array elements don't have to be numbers:
--
-- >>> x1 = (show <$> [1..4]) :: Array '[2,2] String
-- >>> x2 = (show <$> [5..8]) :: Array '[2,2] String
-- >>> pretty x1
-- [["1","2"],
--  ["3","4"]]
--
-- >>> pretty x2
-- [["5","6"],
--  ["7","8"]]
--
-- >>> import Data.List (intercalate)
-- >>> pretty $ dot (intercalate "+" . toList) (\a b -> a <> "*" <> b) x1 x2
-- [["1*5+2*7","1*6+2*8"],
--  ["3*5+4*7","3*6+4*8"]]
--
-- 'dot' allows operation on mis-shaped matrices. The algorithm ignores excess positions within the contracting dimension(s):
--
-- >>> let m23 = [1..6] :: Array '[2,3] Int
-- >>> let m12 = [1,2] :: Array '[1,2] Int
-- >>> shape $ dot sum (*) m23 m12
-- [2,2]
--
-- Find instances of a vector in a matrix
--
-- >>> let cs = fromList ("abacbaab" :: [Char]) :: Array '[4,2] Char
-- >>> let v = fromList ("ab" :: [Char]) :: Vector 2 Char
-- >>> dot (all id) (==) cs v
-- [True,False,False,True]
dot ::
  forall a b c d sa sb s' ss se.
  ( HasShape sa,
    HasShape sb,
    HasShape (sa ++ sb),
    se ~ TakeDims '[Rank sa - 1, Rank sa] (sa ++ sb),
    HasShape se,
    KnownNat (Minimum se),
    KnownNat (Rank sa - 1),
    KnownNat (Rank sa),
    ss ~ '[Minimum se],
    HasShape ss,
    s' ~ DeleteDims '[Rank sa - 1, Rank sa] (sa ++ sb),
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
-- >>> pretty $ mult b (transpose b)
-- [[14,32],
--  [32,77]]
--
-- inner product
--
-- >>> let v = [1..3] :: Array '[3] Int
-- >>> :t mult v v
-- mult v v :: Array '[] Int
--
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
  forall a sa sb s' ss se.
  ( Additive a,
    Multiplicative a,
    HasShape sa,
    HasShape sb,
    HasShape (sa ++ sb),
    se ~ TakeDims '[Rank sa - 1, Rank sa] (sa ++ sb),
    HasShape se,
    KnownNat (Minimum se),
    KnownNat (Rank sa - 1),
    KnownNat (Rank sa),
    ss ~ '[Minimum se],
    HasShape ss,
    s' ~ DeleteDims '[Rank sa - 1, Rank sa] (sa ++ sb),
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
-- s :: Array [2, 2, 2] Int
--
-- >>> pretty $ s
-- [[[2,3],
--   [10,11]],
--  [[14,15],
--   [22,23]]]
--
-- >>> let s = squeeze $ slice (Proxy :: Proxy '[ '[0],'[0],'[0]]) a
-- >>> :t s
-- s :: Array '[] Int
--
-- >>> pretty $ s
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
-- >>> pretty $ a
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
-- >>> pretty $ squeeze a
-- [[[1,2,3,4],
--   [5,6,7,8],
--   [9,10,11,12]],
--  [[13,14,15,16],
--   [17,18,19,20],
--   [21,22,23,24]]]
--
-- >>> pretty $ squeeze ([1] :: Array '[1,1] Double)
-- 1.0
squeeze ::
  forall s t a.
  (t ~ Squeeze s) =>
  Array s a ->
  Array t a
squeeze (Array x) = UnsafeArray x

-- $scalar
-- Scalar specialisations

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
-- toScalar 2 :: FromInteger a => Array '[] a
toScalar :: (HasShape ('[] :: [Nat])) => a -> Array ('[] :: [Nat]) a
toScalar a = UnsafeArray (V.singleton a)

-- | <https://en.wikipedia.org/wiki/Vector_(mathematics_and_physics) Wiki Vector>
type Vector s a = Array '[s] a

-- | Vector specialisation of 'sequent'
--
-- > sequent @'[5]
sequentv :: forall n. (KnownNat n) => Vector n Int
sequentv = sequent

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

instance
  ( Multiplicative a,
    P.Distributive a,
    Subtractive a,
    Eq a,
    ExpField a,
    KnownNat m,
    HasShape '[m, m]
  ) =>
  Divisive (Matrix m m a)
  where
  recip a = invtri (transpose (chol a)) * invtri (chol a)

-- | <https://math.stackexchange.com/questions/1003801/inverse-of-an-invertible-upper-triangular-matrix-of-order-3 Inverse of a triangular> matrix.
invtri :: forall a n. (KnownNat n, ExpField a, Eq a) => Array '[n, n] a -> Array '[n, n] a
invtri a = sum (fmap (l ^) (sequentv :: Vector n Int)) * ti
  where
    ti = undiag (fmap recip (diag a))
    tl = a - undiag (diag a)
    l = negate (ti * tl)

-- | cholesky decomposition
--
-- Uses the <https://en.wikipedia.org/wiki/Cholesky_decomposition#The_Cholesky_algorithm Cholesky-Crout> algorithm.
chol :: (KnownNat n, ExpField a) => Array '[n, n] a -> Array '[n, n] a
chol a =
  let l =
        tabulate
          ( \[i, j] ->
              bool
                ( one
                    / index l [j, j]
                    * ( index a [i, j]
                          - sum
                            ( (\k -> index l [i, k] * index l [j, k])
                                <$> ([zero .. (j - one)] :: [Int])
                            )
                      )
                )
                ( sqrt
                    ( index a [i, i]
                        - sum
                          ( (\k -> index l [j, k] ^ 2)
                              <$> ([zero .. (j - one)] :: [Int])
                          )
                    )
                )
                (i == j)
          )
   in l

-- | Extract specialised to a matrix.
--
-- >>> pretty $ row 1 m
-- [4,5,6,7]
row :: forall m n a. (KnownNat m, KnownNat n, HasShape '[m, n]) => Int -> Matrix m n a -> Vector n a
row i (Array a) = UnsafeArray $ V.slice (i * n) n a
  where
    n = fromIntegral $ natVal @n Proxy

-- | Row extraction checked at type level.
--
-- >>> pretty $ safeRow (Proxy :: Proxy 1) m
-- [4,5,6,7]
--
-- >>> pretty $ safeRow (Proxy :: Proxy 3) m
-- ...
-- ... index outside range
-- ...
safeRow :: forall m n a j. ('True ~ CheckIndex j m, KnownNat j, KnownNat m, KnownNat n, HasShape '[m, n]) => Proxy j -> Matrix m n a -> Vector n a
safeRow _j (Array a) = UnsafeArray $ V.slice (j * n) n a
  where
    n = fromIntegral $ natVal @n Proxy
    j = fromIntegral $ natVal @j Proxy

-- | Extract specialised to a matrix.
--
-- >>> pretty $ col 1 m
-- [1,5,9]
col :: forall m n a. (KnownNat m, KnownNat n, HasShape '[m, n]) => Int -> Matrix m n a -> Vector n a
col i (Array a) = UnsafeArray $ V.generate m (\x -> V.unsafeIndex a (i + x * n))
  where
    m = fromIntegral $ natVal @m Proxy
    n = fromIntegral $ natVal @n Proxy

-- | Column extraction checked at type level.
--
-- >>> pretty $ safeCol (Proxy :: Proxy 1) m
-- [1,5,9]
--
-- >>> pretty $ safeCol (Proxy :: Proxy 4) m
-- ...
-- ... index outside range
-- ...
safeCol :: forall m n a j. ('True ~ CheckIndex j n, KnownNat j, KnownNat m, KnownNat n, HasShape '[m, n]) => Proxy j -> Matrix m n a -> Vector n a
safeCol _j (Array a) = UnsafeArray $ V.generate m (\x -> V.unsafeIndex a (j + x * n))
  where
    m = fromIntegral $ natVal @m Proxy
    n = fromIntegral $ natVal @n Proxy
    j = fromIntegral $ natVal @j Proxy

-- | Matrix multiplication.
--
-- This is dot sum (*) specialised to matrices
--
-- >>> let a = [1,2,3,4] :: Array '[2,2] Int
-- >>> let b = [5,6,7,8] :: Array '[2,2] Int
-- >>> pretty $ a
-- [[1,2],
--  [3,4]]
--
-- >>> pretty $ b
-- [[5,6],
--  [7,8]]
--
-- >>> pretty $ mmult a b
-- [[19,22],
--  [43,50]]
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
