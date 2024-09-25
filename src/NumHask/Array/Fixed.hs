{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- | Arrays with a fixed shape (known shape at compile time).
module NumHask.Array.Fixed
  ( -- * Usage
    -- $usage

    -- * Fixed Arrays
    Array (..),
    unsafeArray,
    validate,
    safeArray,
    array,
    unsafeModifyShape,
    unsafeModifyVector,

    -- * Dimensions
    Dim,
    Dims,

   -- * Dependent type
    SomeArray (..),
    someArray,

    -- * Conversion
    FromVector (..),
    toDynamic,
    with,

    -- * Shape interogation
    shape,
    rank,
    size,
    length,
    isNull,

    -- * Indexing
    index,
    unsafeIndex,
    (!),
    (!?),
    tabulate,
    unsafeTabulate,
    backpermute,
    unsafeBackpermute,

    -- * Scalar
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
    diag,
    undiag,

    -- * Operations
    -- ** Element-level operators
    zipWith,
    modify,
    imap,

    -- ** Operator generalisers
    rowWise,
    colWise,

    -- ** Single-dimension operators
    take,
    takeB,
    drop,
    dropB,
    select,
    insert,
    delete,
    append,
    prepend,
    concatenate,
    couple,
    slice,

    -- * Multi-dimension Operators
    takes,
    takeBs,
    drops,
    dropBs,
    indexes,
    indexesT,
    slices,
    heads,
    lasts,
    tails,
    inits,

    -- * Function application
    extracts,
    reduces,
    joins,
    join,
    traverses,
    maps,
    filters,
    zips,
    modifies,
    diffs,

    -- ** Expansion
    expand,
    expandr,
    contract,
    dot,
    mult,
    windows,

    -- ** Search
    find,
    isPrefixOf,
    isSuffixOf,
    isInfixOf,

    -- * Shape manipulations
    fill,
    cut,
    cutSuffix,
    pad,
    lpad,
    reshape,
    flat,
    repeat,
    cycle,
    rerank,
    reorder,
    squeeze,
    elongate,
    transpose,
    inflate,
    intercalate,
    concats,
    reverses,
    rotate,
    rotates,

    -- * Sorting
    sorts,
    sortsBy,
    orders,
    ordersBy,

    -- * Transmission
    telecasts,
    transmit,

    -- * Row specializations
    pattern (:<),
    cons,
    uncons,
    pattern (:>),
    snoc,
    unsnoc,

    -- * Shape specializations
    Vector,
    vector,
    vector',
    iota,
    Matrix,

    -- * Maths
    uniform,
    invtri,
    inverse,
    chol,

)
where

import Data.Distributive (Distributive (..))
import Data.Functor.Classes
import Data.Functor.Rep
import Data.Vector qualified as V
import Fcf hiding (type (&&), type (+), type (-), type (++))
import Fcf.Data.List
import qualified Fcf
import GHC.TypeNats
import NumHask.Array.Dynamic qualified as D
import NumHask.Array.Shape hiding (concatenate, rank, size, asScalar, asSingleton, squeeze, rotate, reorder, rerank, range)
import NumHask.Array.Shape qualified as S
import NumHask.Prelude as P hiding (Min, take, drop, diff, zipWith, empty, sequence, length, repeat, cycle, find)
import Prelude qualified
import Prettyprinter hiding (dot, fill)
import Data.List qualified as List
import System.Random hiding (uniform)
import System.Random.Stateful hiding (uniform)
import Unsafe.Coerce
import NumHask.Array.Sort
import Test.QuickCheck hiding (tabulate, vector)
import Test.QuickCheck.Instances.Natural ()

-- $setup
--
-- >>> :m -Prelude
-- >>> :set -XDataKinds
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude hiding (cycle, repeat, empty, diff, take, drop, zipWith, find)
-- >>> import NumHask.Array.Fixed as F
-- >>> import NumHask.Array.Shape qualified as S
-- >>> import NumHask.Array.Shape (SNats, Fin (..))
-- >>> import GHC.TypeNats
-- >>> import Prettyprinter hiding (dot,fill)
-- >>> import Data.Functor.Rep
--
-- >>> s = 1 :: Array '[] Int
-- >>> s
-- [1]
-- >>> shape s
-- []
-- >>> pretty s
-- 1
-- >>> let v = range @'[3]
-- >>> pretty v
-- [0,1,2]
-- >>> a = range @[2,3,4]
-- >>> a
-- [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]
-- >>> pretty a
-- [[[0,1,2,3],
--   [4,5,6,7],
--   [8,9,10,11]],
--  [[12,13,14,15],
--   [16,17,18,19],
--   [20,21,22,23]]]
-- >>> toDynamic a
-- UnsafeArray [2,3,4] [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]

-- $usage
--
-- >>> import NumHask.Array.Fixed as F
-- >>> import NumHask.Array.Dynamic qualified as D
-- >>> import NumHask.Array.Shape qualified as S
-- >>> import Prettyprinter (pretty)
-- >>> a = range @[2,3,4]
-- >>> a
-- [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]
-- >>> pretty a
-- [[[0,1,2,3],
--   [4,5,6,7],
--   [8,9,10,11]],
--  [[12,13,14,15],
--   [16,17,18,19],
--   [20,21,22,23]]]
-- >>> toDynamic a
-- UnsafeArray [2,3,4] [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]

-- | A multidimensional array with a type-level shape
--
-- >>> array @[2,3,4] [1..24::Int]
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24]
-- >>> array [1..24] :: Array '[2,3,4] Int
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24]
-- >>> pretty (array [1..24] :: Array '[2,3,4] Int)
-- [[[1,2,3,4],
--   [5,6,7,8],
--   [9,10,11,12]],
--  [[13,14,15,16],
--   [17,18,19,20],
--   [21,22,23,24]]]
--
-- >>> array [1,2,3] :: Array '[2,2] Int
-- *** Exception: NumHaskException {errorMessage = "ShapeMismatch"}
--
-- In many spots, [TypeApplication](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_applications.html) can be cleaner.
--
-- >>> array @[2,3] @Int [1..6]
-- [1,2,3,4,5,6]
--
-- >>> index a (S.UnsafeFins [1,2,3])
-- 23
--
-- >>> tabulate (S.flatten (shape a) . S.fromFins) == a
-- True
type role Array nominal representational

newtype Array (s :: [Nat]) a where
  Array :: V.Vector a -> Array s a
  deriving stock (Functor, Foldable, Generic, Traversable)
  deriving newtype (Eq, Eq1, Ord, Ord1, Show, Show1)

instance (KnownNats s, Show a) => Pretty (Array s a) where
  pretty = pretty . toDynamic

instance
  (KnownNats s) =>
  Data.Distributive.Distributive (Array s)
  where
  distribute :: (KnownNats s, Functor f) => f (Array s a) -> Array s (f a)
  distribute = distributeRep
  {-# INLINE distribute #-}

instance
  forall s.
  (KnownNats s) =>
  Representable (Array s)
  where
  type Rep (Array s) = Fins s

  tabulate f =
    Array . V.generate (S.size s) $ (f . UnsafeFins . shapen s)
    where
      s = valuesOf @s
  {-# INLINE tabulate #-}

  index (Array v) i = V.unsafeIndex v (flatten s (fromFins i))
    where
      s = valuesOf @s
  {-# INLINE index #-}

-- * NumHask heirarchy

instance
  ( Additive a,
    KnownNats s
  ) =>
  Additive (Array s a)
  where
  (+) = liftR2 (+)

  zero = pureRep zero

instance
  ( Subtractive a,
    KnownNats s
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

instance (KnownNats s, JoinSemiLattice a) => JoinSemiLattice (Array s a) where
  (\/) = liftR2 (\/)

instance (KnownNats s, MeetSemiLattice a) => MeetSemiLattice (Array s a) where
  (/\) = liftR2 (/\)

instance (KnownNats s, Subtractive a, Epsilon a) => Epsilon (Array s a) where
  epsilon = konst epsilon

instance (FromInteger a) => FromInteger (Array ('[] :: [Nat]) a) where
  fromInteger x = toScalar (fromInteger x)

instance (FromRational a) => FromRational (Array ('[] :: [Nat]) a) where
  fromRational x = toScalar (fromRational x)

-- | Conversion to and from a `V.Vector`
--
-- Note that conversion of an 'Array' to a vector drops shape information, so that:
--
-- > vectorAs . asVector == id
-- > asVector . vectorAs == flat
--
-- >>> asVector (array [0..5] :: Array [2,3] Int)
-- [0,1,2,3,4,5]
--
-- >>> import Data.Vector qualified as V
-- >>> vectorAs (V.fromList [0..5]) :: Array [2,3] Int
-- [0,1,2,3,4,5]
class FromVector t a | t -> a where
  asVector :: t -> V.Vector a
  vectorAs :: V.Vector a -> t

instance FromVector (V.Vector a) a where
  asVector = id
  vectorAs = id

instance FromVector [a] a where
  asVector = V.fromList
  vectorAs = V.toList

instance FromVector (Array s a) a where
  asVector (Array v) = v
  vectorAs v = Array v

-- | Constructor of an array from a shape and a value without any shape validation.
--
-- >>> unsafeArray [0..4] :: Array [2,3] Int
-- [0,1,2,3,4]
unsafeArray :: (KnownNats s, FromVector t a) => t -> Array s a
unsafeArray (asVector -> v) = Array v

-- | Validate the size and shape of an array.
--
-- >>> validate (unsafeArray [0..4] :: Array [2,3] Int)
-- False
validate :: (KnownNats s) => Array s a -> Bool
validate a = size a == V.length (asVector a)

-- | Construct an Array, checking shape.
--
-- >>> (safeArray [0..23] :: Maybe (Array [2,3,4] Int)) == Just a
-- True
safeArray :: (KnownNats s, FromVector t a) => t -> Maybe (Array s a)
safeArray v =
  bool Nothing (Just a) (validate a)
  where
    a = unsafeArray v

-- | Construct an Array, checking shape.
--
-- >>> array [0..22] :: Array [2,3,4] Int
-- *** Exception: NumHaskException {errorMessage = "ShapeMismatch"}
array :: forall s a t. (KnownNats s, FromVector t a) => t -> Array s a
array v =
  fromMaybe (throw (NumHaskException "ShapeMismatch")) (safeArray v)

-- | Unsafely modify an array shape.
--
-- >>> pretty (unsafeModifyShape @[3,2] (array @[2,3] @Int [0..5]))
-- [[0,1],
--  [2,3],
--  [4,5]]
unsafeModifyShape :: forall s' s a. (KnownNats s, KnownNats s') => Array s a -> Array s' a
unsafeModifyShape a = unsafeArray (asVector a)

-- | Unsafely modify an array vector.
--
-- >>> import Data.Vector qualified as V
-- >>> pretty (unsafeModifyVector (V.map (+1)) (array [0..5] :: Array [2,3] Int))
-- [[1,2,3],
--  [4,5,6]]
unsafeModifyVector :: (KnownNats s) => (FromVector u a) => (FromVector v b) => (u -> v) -> Array s a -> Array s b
unsafeModifyVector f a = unsafeArray (asVector (f (vectorAs (asVector a))))

-- | Representation of an index into a shape (a type-level [Nat]). The index is a dimension of the shape.
type Dim = SNat

-- | Representation of indexes into a shape (a type-level [Nat]). The indexes are dimensions of the shape.
type Dims = SNats


-- | A fixed Array with a hidden shape.
--
-- The library design encourages the use of dynamic arrays in preference to dependent-type styles such as this. In particular, no attempt has been made to prove to the compiler that a particular Shape (resulting from any of the supplied functions) exists. Life is short.
data SomeArray a = forall s. SomeArray (SNats s) (Array s a)

deriving instance (Show a) => Show (SomeArray a)

instance Functor SomeArray
  where
    fmap f (SomeArray sn a) = SomeArray sn (fmap f a)

instance Foldable SomeArray
  where
    foldMap f (SomeArray _ a) = foldMap f a

someArray :: forall s t a. FromVector t a => SNats s -> t -> SomeArray a
someArray s t = SomeArray s (Array (asVector t))

-- |
-- > P.take 4 <$> sample' arbitrary :: IO [SomeArray Int]
-- [SomeArray SNats @'[] [0],SomeArray SNats @'[0] [],SomeArray SNats @[1, 1] [1],SomeArray SNats @[5, 1, 4] [2,1,0,2,-6,0,5,6,-1,-4,0,5,-1,6,4,-6,1,0,3,-1]]
instance (Arbitrary a) => Arbitrary (SomeArray a) where
  arbitrary = do
    s <- arbitrary :: Gen [Small Nat]
    let s' = Prelude.take 3 (getSmall <$> s)
    v <- V.replicateM (product (Prelude.fromIntegral <$> s')) arbitrary
    withSomeSNats s' $ \sn -> pure (someArray sn v)

-- | convert to a dynamic array with shape at the value level.
--
-- >>> toDynamic a
-- UnsafeArray [2,3,4] [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]
toDynamic :: (KnownNats s) => Array s a -> D.Array a
toDynamic a = D.array (shape a) (asVector a)

-- | Use a dynamic array in a fixed context.
--
-- FIXME: rethink this structure
-- >>> import qualified NumHask.Array.Dynamic as D
-- >>> with (D.range [2,3,4]) (F.indexes (S.SNats @[0,1]) (S.UnsafeFins [1,1]) :: F.Array [2,3,4] Int -> F.Array '[4] Int)
-- [16,17,18,19]
with ::
  forall a r s.
  (KnownNats s) =>
  D.Array a ->
  (Array s a -> r) ->
  r
with (D.UnsafeArray _ v) f = f (Array v)

-- | Get shape of an Array as a value.
--
-- >>> shape a
-- [2,3,4]
shape :: forall a s. (KnownNats s) => Array s a -> [Int]
shape _ = valuesOf @s
{-# INLINE shape #-}

-- | Get rank of an Array as a value.
--
-- >>> rank a
-- 3
rank :: forall a s. (KnownNats s) => Array s a -> Int
rank = S.rank . shape
{-# INLINE rank #-}

-- | Get size of an Array as a value.
--
-- >>> size a
-- 24
size :: forall a s. (KnownNats s) => Array s a -> Int
size = S.size . shape
{-# INLINE size #-}

-- | Number of rows (first dimension size) in an Array. As a convention, a scalar value is still a single row.
--
-- >>> F.length a
-- 2
length :: (KnownNats s) => Array s a -> Int
length a = case shape a of
  [] -> one
  (x : _) -> x

-- | Is the Array empty (has zero number of elements).
--
-- >>> isNull (array [] :: Array [1,0] ())
-- True
-- >>> isNull (array [4] :: Array '[] Int)
-- False
isNull :: (KnownNats s) => Array s a -> Bool
isNull = (zero ==) . size

-- | Extract an element at an index, unsafely.
--
-- >>> unsafeIndex a [1,2,3]
-- 23
unsafeIndex :: (KnownNats s) => Array s a -> [Int] -> a
unsafeIndex a xs = index a (UnsafeFins xs)

-- | Extract an element at an index, unsafely.
--
-- >>> a ! [1,2,3]
-- 23
(!) :: (KnownNats s) => Array s a -> [Int] -> a
(!) a xs = index a (UnsafeFins xs)

-- | Extract an element at an index, safely.
--
-- >>> a !? [1,2,3]
-- Just 23
-- >>> a !? [2,3,1]
-- Nothing
(!?) :: (KnownNats s) => Array s a -> [Int] -> Maybe a
(!?) a xs = index a <$> toFins xs

-- | Tabulate unsafely
unsafeTabulate :: (KnownNats s) => ([Int] -> a) -> Array s a
unsafeTabulate f = tabulate (f . fromFins)

-- | Safe backpermute
backpermute :: (KnownNats s, KnownNats s') => (Fins s' -> Fins s) -> Array s a -> Array s' a
backpermute f a = tabulate (index a . f)
{-# INLINEABLE backpermute #-}

{- RULES
   "backpermute/backpermute" forall f f' (a :: forall a. Array a)). backpermute f (backpermute f' a) == backpermute (f . f') a
-}

-- | Unsafe backpermute
unsafeBackpermute :: (KnownNats s, KnownNats s') => ([Int] -> [Int]) -> Array s a -> Array s' a
unsafeBackpermute f a = tabulate (index a . UnsafeFins . f . fromFins)

{- RULES
   "unsafeBackpermute/unsafeBackpermute" forall f f' (a :: forall a. Array a)). unsafeBackpermute f (unsafeBackpermute f' a) == unsafeBackpermute (f . f') a
-}

-- | Unwrapping scalars is probably a performance bottleneck.
--
-- >>> s = array @'[] @Int [3]
-- >>> :t fromScalar s
-- fromScalar s :: Int
fromScalar :: Array '[] a -> a
fromScalar a = index a (UnsafeFins [])

-- | Convert a number to a scalar.
--
-- >>> :t toScalar @Int 2
-- toScalar @Int 2 :: Array '[] Int
toScalar :: a -> Array '[] a
toScalar a = Array (V.singleton a)

-- | Is the Array a Scalar?
--
-- >>> isScalar (toScalar (2::Int))
-- True
isScalar :: (KnownNats s) => Array s a -> Bool
isScalar a = rank a == zero

-- | Convert scalars to dimensioned arrays.
--
-- >>> asSingleton (toScalar 4)
-- [4]
asSingleton :: (KnownNats s, KnownNats s', s' ~ Eval (AsSingleton s)) => Array s a -> Array s' a
asSingleton = unsafeModifyShape

-- | Convert arrays with shape [1] to scalars.
--
-- >>> pretty (asScalar (singleton 3))
-- 3
asScalar :: (KnownNats s, KnownNats s', s' ~ Eval (AsScalar s)) => Array s a -> Array s' a
asScalar = unsafeModifyShape

-- * Creation
-- | An array with no elements.
--
-- >>> toDynamic empty
-- UnsafeArray [0] []
empty :: Array '[0] a
empty = array []

-- | A flat enumeration.
--
-- >>> pretty (range :: Array [2,3] Int)
-- [[0,1,2],
--  [3,4,5]]
range :: forall s. (KnownNats s) => Array s Int
range = tabulate (S.flatten (valuesOf @s) . fromFins)

-- | Indices of an array shape.
--
-- >>> pretty $ indices @[3,3]
-- [[[0,0],[0,1],[0,2]],
--  [[1,0],[1,1],[1,2]],
--  [[2,0],[2,1],[2,2]]]
indices :: (KnownNats s) => Array s [Int]
indices = tabulate fromFins

-- | The identity array.
--
-- >>> pretty $ ident @[3,3]
-- [[1,0,0],
--  [0,1,0],
--  [0,0,1]]
ident :: (KnownNats s, Ring a) => Array s a
ident = tabulate (bool zero one . S.isDiag . fromFins)

-- | Create an array composed of a single value.
--
-- >>> pretty $ konst @[3,2] one
-- [[1,1],
--  [1,1],
--  [1,1]]
konst :: (KnownNats s) => a -> Array s a
konst a = tabulate (const a)

-- | Create an array of shape [1].
--
-- >>> pretty $ singleton one
-- [1]
singleton :: a -> Array '[1] a
singleton a = unsafeArray (V.singleton a)

-- | Extract the diagonal of an array.
--
-- >>> pretty $ diag (ident @[3,3])
-- [1,1,1]
diag ::
  forall s' a s.
  ( KnownNats s,
    KnownNats s',
    s' ~ Eval (MinDim s)
  ) =>
  Array s a ->
  Array s' a
diag a = unsafeBackpermute (replicate (rank a) . getDim 0) a

-- | Expand the array to form a diagonal array
--
-- >>> pretty $ undiag (range @'[3])
-- [[0,0,0],
--  [0,1,0],
--  [0,0,2]]
undiag ::
  forall s' a s.
  ( KnownNats s,
    KnownNats s',
    s' ~ Eval ((++) s s),
    Additive a
  ) =>
  Array s a ->
  Array s' a
undiag a = tabulate ((\xs -> bool zero (index a (UnsafeFins $ pure $ getDim 0 (fromFins xs))) (isDiag (fromFins xs))))

-- | Zip two arrays at an element level. Could also be called liftS2 or sometink like that.
--
-- > zipWith == \f a b -> zips (range (rank a)) (\f a b -> f (D.toScalar a) (D.toScalar b))
--
-- >>> zipWith (-) v v
-- [0,0,0]
zipWith :: (KnownNats s) => (a -> b -> c) -> Array s a -> Array s b -> Array s c
zipWith f (asVector -> a) (asVector -> b) = unsafeArray (V.zipWith f a b)

-- | Modify a single value at an index.
--
-- >>> pretty $ modify (S.UnsafeFins [0,0]) (const 100) (range @[3,2])
-- [[100,1],
--  [2,3],
--  [4,5]]
modify :: (KnownNats s) => Fins s -> (a -> a) -> Array s a -> Array s a
modify ds f a = tabulate (\s -> bool id f (s == ds) (index a s))

-- | Maps an index function at element-level.
--
-- >>> pretty $ imap (\xs x -> x - sum xs) a
-- [[[0,0,0,0],
--   [3,3,3,3],
--   [6,6,6,6]],
--  [[11,11,11,11],
--   [14,14,14,14],
--   [17,17,17,17]]]
imap ::
  (KnownNats s) =>
  ([Int] -> a -> b) ->
  Array s a ->
  Array s b
imap f a = zipWith f indices a

-- | Apply a function that takes dimensions and (type-level) parameters and applies a parameters to the initial dimensions. ie
--
-- > rowWise f xs = f [0..rank xs - 1] xs
--
-- >>> toDynamic $ rowWise indexesT (S.SNats @[1,0]) a
-- UnsafeArray [4] [12,13,14,15]
rowWise ::
  forall a ds s s' xs proxy.
  ( KnownNats s
  , KnownNats ds
  , ds ~ Eval (DimsOf xs)
  ) =>
  (Dims ds -> proxy xs -> Array s a -> Array s' a) ->
  proxy xs -> Array s a -> Array s' a
rowWise f xs a = f (SNats @ds) xs a

-- | Apply a function that takes a (dimension,parameter) list and applies a parameter list to the the last dimensions (in reverse). ie
--
-- > colWise f xs = f (List.reverse [0 .. (rank a - 1)]) xs
--
-- >>> toDynamic $ colWise indexesT (S.SNats @[1,0]) a
-- UnsafeArray [2] [1,13]
colWise ::
  forall a ds s s' xs proxy.
  ( KnownNats s
  , KnownNats ds
  , ds ~ Eval (EndDimsOf xs s)) =>
  (Dims ds -> proxy xs -> Array s a -> Array s' a) ->
  proxy xs -> Array s a -> Array s' a
colWise f xs a = f (SNats @ds) xs a

-- | Take the top-most elements across the specified dimension.
--
-- >>> pretty $ take (SNat @2) (SNat @1) a
-- [[[0],
--   [4],
--   [8]],
--  [[12],
--   [16],
--   [20]]]
take ::
  forall d t s s' a.
  ( KnownNats s,
    KnownNats s',
    s' ~ Eval (TakeDim d t s)
  ) =>
  Dim d ->
  SNat t ->
  Array s a ->
  Array s' a
take _ _ a = unsafeBackpermute id a

-- | Take the bottom-most elements across the specified dimension.
--
-- >>> pretty $ takeB (SNat @2) (SNat @1) a
-- [[[3],
--   [7],
--   [11]],
--  [[15],
--   [19],
--   [23]]]
takeB ::
  forall s s' a d t.
  ( KnownNats s,
    KnownNats s',
    s' ~ Eval (TakeDim d t s)
  ) =>
  Dim d ->
  SNat t ->
  Array s a ->
  Array s' a
takeB SNat SNat a = unsafeBackpermute (\s -> modifyDim (valueOf @d) (\x -> x + (getDim (valueOf @d) (shape a)) - (valueOf @t)) s) a

-- | Drop the top-most elements across the specified dimension.
--
-- >>> pretty $ drop (SNat @2) (SNat @1) a
-- [[[1,2,3],
--   [5,6,7],
--   [9,10,11]],
--  [[13,14,15],
--   [17,18,19],
--   [21,22,23]]]
drop ::
  forall s s' a d t.
  ( KnownNats s,
    KnownNats s',
    Eval (DropDim d t s) ~ s'
  ) =>
  Dim d ->
  SNat t ->
  Array s a ->
  Array s' a
drop SNat SNat a = unsafeBackpermute (S.modifyDim (valueOf @d) (\x -> x + valueOf @t)) a

-- | Drop the bottom-most elements across the specified dimension.
--
-- >>> pretty $ dropB (SNat @2) (SNat @1) a
-- [[[0,1,2],
--   [4,5,6],
--   [8,9,10]],
--  [[12,13,14],
--   [16,17,18],
--   [20,21,22]]]
dropB ::
  forall s s' a d t.
  ( KnownNats s,
    KnownNats s',
    Eval (DropDim d t s) ~ s'
  ) =>
  Dim d ->
  SNat t ->
  Array s a ->
  Array s' a
dropB _ _ a = unsafeBackpermute id a

-- | Select an index along a dimension.
--
-- >>> let s = select (SNat @2) (S.fin @4 3) a
-- >>> pretty s
-- [[3,7,11],
--  [15,19,23]]
select ::
  forall d a p s s'.
  (KnownNats s,
   KnownNats s',
   s' ~ Eval (DeleteDim d s),
   p ~ Eval (GetDim d s)
  ) =>
  Dim d ->
  Fin p ->
  Array s a ->
  Array  s' a
select SNat p a = unsafeBackpermute (S.insertDim (valueOf @d) (fromFin p)) a

-- | Insert along a dimension at a position.
--
-- >>> pretty $ insert (SNat @2) (UnsafeFin 0) a (konst @[2,3] 0)
-- [[[0,0,1,2,3],
--   [0,4,5,6,7],
--   [0,8,9,10,11]],
--  [[0,12,13,14,15],
--   [0,16,17,18,19],
--   [0,20,21,22,23]]]
-- >>> toDynamic $ insert (SNat @0) (UnsafeFin 0) (toScalar 1) (toScalar 2)
-- UnsafeArray [2] [2,1]
insert ::
  forall s' s si d p a.
  (KnownNats s,
   KnownNats si,
   KnownNats s',
   s' ~ Eval (IncAt d s),
   p ~ Eval (GetDim d s),
   True ~ Eval (InsertOk d s si)
  ) =>
  Dim d ->
  Fin p ->
  Array s a ->
  Array si a ->
  Array s' a
insert SNat i a b = tabulate go
  where
    go s
      | getDim d s' == fromFin i = index b (UnsafeFins (deleteDim d s'))
      | getDim d s' < fromFin i = index a (UnsafeFins s')
      | otherwise = index a (UnsafeFins (decAt d s'))
      where s' = fromFins s
    d = valueOf @d

-- | Delete along a dimension at a position.
--
-- >>> pretty $ delete (SNat @2) (UnsafeFin 3) a
-- [[[0,1,2],
--   [4,5,6],
--   [8,9,10]],
--  [[12,13,14],
--   [16,17,18],
--   [20,21,22]]]
delete ::
  forall d s s' p a.
  (KnownNats s,
   KnownNats s',
   s' ~ Eval (DecAt d s),
   p ~ 1 + Eval (GetDim d s)) =>
  Dim d ->
  Fin p ->
  Array s a ->
  Array s' a
delete SNat p a = unsafeBackpermute (\s -> bool (incAt d s) s (getDim d s < fromFin p)) a
  where
    d = valueOf @d

-- | Insert along a dimension at the end.
--
-- >>> pretty $ append (SNat @2) a (konst @[2,3] 0)
-- [[[0,1,2,3,0],
--   [4,5,6,7,0],
--   [8,9,10,11,0]],
--  [[12,13,14,15,0],
--   [16,17,18,19,0],
--   [20,21,22,23,0]]]
append ::
  forall a d s si s'.
  ( KnownNats s,
    KnownNats si,
    KnownNats s',
    s' ~ Eval (IncAt d s),
    True ~ Eval (InsertOk d s si)
  ) =>
  Dim d ->
  Array s a ->
  Array si a ->
  Array s' a
append (SNat :: SNat d) = insert (SNat @d) (UnsafeFin (getDim (valueOf @d) (valuesOf @s)))

-- | Insert along a dimension at the beginning.
--
-- >>> pretty $ prepend (SNat @2) (konst @[2,3] 0) a
-- [[[0,0,1,2,3],
--   [0,4,5,6,7],
--   [0,8,9,10,11]],
--  [[0,12,13,14,15],
--   [0,16,17,18,19],
--   [0,20,21,22,23]]]
prepend ::
  forall a d s si s'.
  ( KnownNats s,
    KnownNats si,
    KnownNats s',
    s' ~ Eval (IncAt d s),
    True ~ Eval (InsertOk d s si)
  ) =>
  Dim d ->
  Array si a ->
  Array s a ->
  Array s' a
prepend d a b = insert d (UnsafeFin 0) b a

-- | Concatenate along a dimension.
--
-- >>> shape $ concatenate (SNat @1) a a
-- [2,6,4]
-- >>> toDynamic $ concatenate (SNat @0) (toScalar 1) (toScalar 2)
-- UnsafeArray [2] [1,2]
-- >>> toDynamic $ concatenate (SNat @0) (array @'[1] [0]) (array @'[3] [1..3])
-- UnsafeArray [4] [0,1,2,3]
concatenate ::
  forall a s0 s1 d s.
  ( KnownNats s0,
    KnownNats s1,
    KnownNats s,
    Eval (Concatenate d s0 s1) ~ s
  ) =>
  Dim d ->
  Array s0 a ->
  Array s1 a ->
  Array s a
concatenate SNat a0 a1 = tabulate (go . fromFins)
  where
    go s =
      bool
        (index a0 (UnsafeFins s))
        ( index
            a1
            ( UnsafeFins $ insertDim
                d'
                (getDim d' s - getDim d' ds0)
                (deleteDim d' s)
            )
        )
        (getDim d' s >= getDim d' ds0)
    ds0 = shape a0
    d' = valueOf @d

-- | Combine two arrays as rows of a new array.
--
-- >>> pretty $ couple (array @'[3] [1,2,3]) (array @'[3] @Int [4,5,6])
-- [[1,2,3],
--  [4,5,6]]
-- >>> couple (toScalar @Int 0) (toScalar 1)
-- [0,1]
couple :: forall a s s' se.
  (KnownNats s,
   KnownNats s',
   KnownNats se,
   s' ~ Eval (Concatenate 0 se se),
   se ~ Eval (InsertDim 0 1 s)
  ) =>
  Array s a -> Array s a -> Array s' a
couple a a' = concatenate (SNat @0) (elongate (SNat @0) a) (elongate (SNat @0) a')

-- | Slice along a dimension with the supplied (offset, length).
--
-- >>> pretty $ slice (SNat @2) (SNat @1) (SNat @2) a
-- [[[1,2],
--   [5,6],
--   [9,10]],
--  [[13,14],
--   [17,18],
--   [21,22]]]
slice ::
  forall a d off l s s'.
  (KnownNats s,
   KnownNats s',
   s' ~ Eval (SetDim d l s),
   Eval (SliceOk d off l s) ~ True) =>
  Dim d ->
  SNat off ->
  SNat l ->
  Array s a ->
  Array s' a
slice SNat SNat _ a = unsafeBackpermute (S.modifyDim (valueOf @d) (+ (valueOf @off))) a

-- | Rotate an array along a dimension.
--
-- >>> pretty $ rotate (SNat @1) 2 a
-- [[[8,9,10,11],
--   [0,1,2,3],
--   [4,5,6,7]],
--  [[20,21,22,23],
--   [12,13,14,15],
--   [16,17,18,19]]]
rotate ::
  forall d s a.
  (KnownNats s) =>
  Dim d ->
  Int ->
  Array s a ->
  Array s a
rotate SNat r a = unsafeBackpermute (rotateIndex (valueOf @d) r (shape a)) a

-- * multi-dimensional operators

-- | Takes the top-most elements across the supplied dimension,n tuples.
--
-- >>> pretty $ takes (S.SNats @[0,1]) (S.SNats @[1,2]) a
-- [[[0,1,2,3],
--   [4,5,6,7]]]
takes ::
  forall ds xs s' s a.
  ( KnownNats s,
    KnownNats s',
    s' ~ Eval (SetDims ds xs s)
  ) =>
  Dims ds ->
  SNats xs ->
  Array s a ->
  Array s' a
takes _ _ a = unsafeBackpermute id a

-- | Takes the bottom-most elements across the supplied dimension,n tuples.
--
-- >>> pretty (takeBs (S.SNats @[0,1]) (S.SNats @[1,2]) a)
-- [[[16,17,18,19],
--   [20,21,22,23]]]
takeBs ::
  forall s' s a ds xs.
  ( KnownNats s,
    KnownNats s',
    KnownNats ds,
    KnownNats xs,
    s' ~ Eval (SetDims ds xs s)
  ) =>
  Dims ds ->
  SNats xs ->
  Array s a ->
  Array s' a
takeBs _ _ a = unsafeBackpermute (List.zipWith (+) start) a
  where
    start = List.zipWith (-) (shape a) (S.setDims (valuesOf @ds) (valuesOf @xs) (shape a))

-- | Drops the top-most elements across dimension,n tuples.
--
-- >>> pretty $ drops (S.SNats @[0,2]) (S.SNats @[1,3]) a
-- [[[15],
--   [19],
--   [23]]]
drops ::
  forall ds xs s' s a.
  ( KnownNats s,
    KnownNats s',
    KnownNats ds,
    KnownNats xs,
    s' ~ Eval (DropDims ds xs s)
  ) =>
  Dims ds ->
  SNats xs ->
  Array s a ->
  Array s' a
drops _ _ a = unsafeBackpermute (List.zipWith (+) start) a
  where
    start = List.zipWith (-) (valuesOf @s) (valuesOf @s')

-- | Drops the bottom-most elements across dimension,n tuples.
--
-- >>> pretty $ dropBs (S.SNats @[0,2]) (S.SNats @[1,3]) a
-- [[[0],
--   [4],
--   [8]]]
dropBs ::
  forall s' s ds xs a.
  ( KnownNats s,
    KnownNats s',
    KnownNats ds,
    KnownNats xs,
    s' ~ Eval (DropDims ds xs s)
  ) =>
  Dims ds ->
  SNats xs ->
  Array s a ->
  Array s' a
dropBs _ _ a = unsafeBackpermute id a

-- | Select by an index along dimensions.
--
-- >>> pretty $ indexes (S.SNats @[0,1]) (S.UnsafeFins [1,1]) a
-- [16,17,18,19]
indexes ::
  forall ds s s' xs a.
  ( KnownNats s,
    KnownNats s',
    s' ~ Eval (DeleteDims ds s),
    xs ~ Eval (GetDims ds s)
  ) =>
  Dims ds ->
  Fins xs ->
  Array s a ->
  Array s' a
indexes SNats xs a = unsafeBackpermute (S.insertDims (valuesOf @ds) (fromFins xs)) a

--- | Select by dimensions and indexes, supplying indexes as a type.
---
-- >>> pretty $ indexesT (S.SNats @[0,1]) (S.SNats @[1,1]) a
-- [16,17,18,19]
indexesT ::
  forall ds xs s s' a.
  ( KnownNats s,
    KnownNats ds,
    KnownNats xs,
    KnownNats s',
    s' ~ Eval (DeleteDims ds s),
    True ~ Eval (IsFins xs =<< GetDims ds s)
  ) =>
  Dims ds ->
  SNats xs ->
  Array s a ->
  Array s' a
indexesT ds _ a = indexes ds (UnsafeFins $ valuesOf @xs) a

-- | Slice along dimensions with the supplied offsets and lengths.
--
-- >>> pretty $ slices (S.SNats @'[2]) (S.SNats @'[1]) (S.SNats @'[2]) a
-- [[[1,2],
--   [5,6],
--   [9,10]],
--  [[13,14],
--   [17,18],
--   [21,22]]]
slices ::
  forall a ds ls offs s s'.
  (KnownNats s,
   KnownNats s',
   KnownNats ds,
   KnownNats ls,
   KnownNats offs,
   Eval (SlicesOk ds offs ls s) ~ True,
   Eval (SetDims ds ls s) ~ s') =>
  Dims ds ->
  SNats offs ->
  SNats ls ->
  Array s a ->
  Array s' a
slices _ _ _ a = unsafeBackpermute (List.zipWith (+) o) a
  where
    o = S.setDims (valuesOf @ds) (valuesOf @offs) (replicate (rank a) 0)

-- | Select the first element along the supplied dimensions
--
-- >>> pretty $ heads (S.SNats @[0,2]) a
-- [0,4,8]
heads ::
  forall a ds s s'.
  ( KnownNats s,
    KnownNats s',
    KnownNats ds,
    s' ~ Eval (DeleteDims ds s)) =>
  Dims ds ->
  Array s a ->
  Array s' a
heads ds a = indexes ds (UnsafeFins $ replicate (rankOf @ds) 0) a

-- | Select the last element along the supplied dimensions
--
-- >>> pretty $ lasts (S.SNats @[0,2]) a
-- [15,19,23]
lasts ::
  forall ds s s' a.
  ( KnownNats s,
    KnownNats ds,
    KnownNats s',
    s' ~ Eval (DeleteDims ds s)
  ) =>
  Dims ds ->
  Array s a ->
  Array s' a
lasts ds a = indexes ds (UnsafeFins lastds) a
  where
    lastds = (\i -> getDim i (shape a) - 1) <$> (valuesOf @ds)

-- | Select the tail elements along the supplied dimensions
--
-- >>> pretty $ tails (S.SNats @[0,2]) a
-- [[[13,14,15],
--   [17,18,19],
--   [21,22,23]]]
tails ::
  forall ds os s s' a ls.
  ( KnownNats s,
    KnownNats ds,
    KnownNats s',
    KnownNats ls,
    KnownNats os,
    Eval (SlicesOk ds os ls s) ~ True,
    os ~ Eval (Replicate (Eval (Rank ds)) 1),
    ls ~ Eval (GetLastPositions ds s),
    s' ~ Eval (SetDims ds ls s)
  ) =>
  Dims ds ->
  Array s a ->
  Array s' a
tails ds a = slices ds (SNats @os) (SNats @ls) a

-- | Select the init elements along the supplied dimensions
--
-- >>> pretty $ inits (S.SNats @[0,2]) a
-- [[[0,1,2],
--   [4,5,6],
--   [8,9,10]]]
inits ::
  forall ds os s s' a ls.
  ( KnownNats s,
    KnownNats ds,
    KnownNats s',
    KnownNats ls,
    KnownNats os,
    Eval (SlicesOk ds os ls s) ~ True,
    os ~ Eval (Replicate (Eval (Rank ds)) 0),
    ls ~ Eval (GetLastPositions ds s),
    s' ~ Eval (SetDims ds ls s)
  ) =>
  Dims ds ->
  Array s a ->
  Array s' a
inits ds a = slices ds (SNats @os) (SNats @ls) a

-- | Extracts dimensions to an outer layer.
--
-- > a == (fromScalar <$> extracts [0..rank a] a)
--
-- >>> pretty $ shape <$> extracts (S.SNats @'[0]) a
-- [[3,4],[3,4]]
extracts ::
  forall ds st si so a.
  ( KnownNats st,
    KnownNats ds,
    KnownNats si,
    KnownNats so,
    si ~ Eval (DeleteDims ds st),
    so ~ Eval (GetDims ds st)
  ) =>
  Dims ds ->
  Array st a ->
  Array so (Array si a)
extracts ds a = tabulate (\s -> indexes ds s a)

-- | Reduce along specified dimensions, using the supplied fold.
--
-- >>> pretty $ reduces (S.SNats @'[0]) sum a
-- [66,210]
-- >>> pretty $ reduces (S.SNats @[0,2]) sum a
-- [[12,15,18,21],
--  [48,51,54,57]]
--
reduces ::
  forall ds st si so a b.
  ( KnownNats st,
    KnownNats ds,
    KnownNats si,
    KnownNats so,
    si ~ Eval (DeleteDims ds st),
    so ~ Eval (GetDims ds st)
  ) =>
  Dims ds ->
  (Array si a -> b) ->
  Array st a ->
  Array so b
reduces ds f a = fmap f (extracts ds a)

-- | Join inner and outer dimension layers by supplied dimensions. No checks on shape.
--
-- >>> let e = extracts (S.SNats @[1,0]) a
-- >>> let j = joins (S.SNats @[1,0]) e
-- >>> a == j
-- True
joins ::
  forall a ds si so st.
  (KnownNats ds,
   KnownNats st,
   KnownNats si,
   KnownNats so,
   Eval (InsertDims ds so si) ~ st) =>
  Dims ds ->
  Array so (Array si a) ->
  Array st a
joins _ a = tabulate go
  where
    go s = index (index a (UnsafeFins $ S.getDims (valuesOf @ds) (fromFins s))) (UnsafeFins $ S.deleteDims (valuesOf @ds) (fromFins s))

-- | Join inner and outer dimension layers in outer dimension order.
--
-- >>> a == join (extracts (S.SNats @[0,1]) a)
-- True
join ::
  forall a si so st ds.
  (KnownNats st,
   KnownNats si,
   KnownNats so,
   KnownNats ds,
   ds ~ Eval (DimsOf so),
   st ~ Eval (InsertDims ds so si)
  ) =>
  Array so (Array si a) ->
  Array st a
join a = joins (SNats @ds) a
  where
    -- go s = index (index a (UnsafeFins $ S.getDims ds (fromFins s))) (UnsafeFins $ S.deleteDims ds (fromFins s))
    -- ds = [0..rankOf @so - 1]

-- | Traverse along specified dimensions.
--
-- FIXME: Needs example.
traverses ::
  (Applicative f,
   KnownNats s,
   KnownNats si,
   KnownNats so,
   si ~ Eval (GetDims ds s),
   so ~ Eval (DeleteDims ds s),
   s ~ Eval (InsertDims ds si so)
   ) =>
  Dims ds ->
  (a -> f b) ->
  Array s a ->
  f (Array s b)
traverses (SNats :: SNats ds) f a = joins (SNats @ds) <$> traverse (traverse f) (extracts (SNats :: SNats ds) a)

-- | Maps a function along specified dimensions.
--
-- > :t maps (S.SNats @'[1]) transpose a
-- maps (transpose) (S.SNats @'[1]) a :: Array [4, 3, 2] Int
maps ::
  forall ds s s' si si' so a b.
  ( KnownNats s,
    KnownNats s',
    KnownNats si,
    KnownNats si',
    KnownNats so,
    si ~ Eval (DeleteDims ds s),
    so ~ Eval (GetDims ds s),
    s' ~ Eval (InsertDims ds so si'),
    s ~ Eval (InsertDims ds so si)
  ) =>
  Dims ds ->
  (Array si a -> Array si' b) ->
  Array s a ->
  Array s' b
maps SNats f a = joins (SNats @ds) (fmap f (extracts (SNats @ds) a))

-- | Filters along specified dimensions (which are flattened as a dynamic array).
--
-- >>> pretty $ filters (S.SNats @[0,1]) (any ((==0) . (`mod` 7))) a
-- [[0,1,2,3],[4,5,6,7],[12,13,14,15],[20,21,22,23]]
filters ::
  forall ds si so a.
  ( KnownNats si,
    KnownNats so,
    si ~ Eval (DeleteDims ds so),
    KnownNats (Eval (GetDims ds so))
  ) =>
  Dims ds ->
  (Array si a -> Bool) ->
  Array so a ->
  D.Array (Array si a)
filters SNats p a = D.asArray $ V.filter p $ asVector (extracts (SNats @ds) a)

-- | Zips two arrays with a function along specified dimensions.
--
-- >>> pretty $ zips (S.SNats @[0,1]) (zipWith (,)) a (reverses (S.SNats @'[0]) a)
-- [[[(0,12),(1,13),(2,14),(3,15)],
--   [(4,16),(5,17),(6,18),(7,19)],
--   [(8,20),(9,21),(10,22),(11,23)]],
--  [[(12,0),(13,1),(14,2),(15,3)],
--   [(16,4),(17,5),(18,6),(19,7)],
--   [(20,8),(21,9),(22,10),(23,11)]]]
zips ::
  forall ds s s' si si' so a b c.
  ( KnownNats s,
    KnownNats s',
    KnownNats si,
    KnownNats si',
    KnownNats so,
    si ~ Eval (DeleteDims ds s),
    so ~ Eval (GetDims ds s),
    s' ~ Eval (InsertDims ds so si'),
    s ~ Eval (InsertDims ds so si)
  ) =>
  Dims ds ->
  (Array si a -> Array si b -> Array si' c) ->
  Array s a ->
  Array s b ->
  Array s' c
zips SNats f a b = joins (SNats @ds) (zipWith f (extracts (SNats @ds) a) (extracts (SNats @ds) b))

-- | Modify using the supplied function along dimension and positions.
--
-- >>> pretty $ modifies (fmap (100+)) (S.SNats @'[2]) (S.UnsafeFins [0]) a
-- [[[100,1,2,3],
--   [104,5,6,7],
--   [108,9,10,11]],
--  [[112,13,14,15],
--   [116,17,18,19],
--   [120,21,22,23]]]
modifies ::
  forall a si s ds so.
  ( KnownNats s,
    KnownNats si,
    KnownNats so,
    si ~ Eval (DeleteDims ds s),
    so ~ Eval (GetDims ds s),
    s ~ Eval (InsertDims ds so si)) =>
  (Array si a -> Array si a) ->
  Dims ds ->
  Fins so ->
  Array s a ->
  Array s a
modifies f SNats ps a = joins (SNats @ds) $ modify ps f (extracts (SNats @ds) a)

-- | Apply a binary function between successive slices, across (dimension, lag) tuples
--
-- >>> pretty $ diffs (S.SNats @'[1]) (S.SNats @'[1]) (zipWith (-)) a
-- [[[4,4,4,4],
--   [4,4,4,4]],
--  [[4,4,4,4],
--   [4,4,4,4]]]
diffs ::
  forall a b ds ls si si' st st' so postDrop.
  ( KnownNats ls,
    KnownNats si,
    KnownNats si',
    KnownNats st,
    KnownNats st',
    KnownNats so,
    KnownNats postDrop,
    si ~ Eval (DeleteDims ds postDrop),
    so ~ Eval (GetDims ds postDrop),
    st' ~ Eval (InsertDims ds so si'),
    postDrop ~ Eval (InsertDims ds so si),
    postDrop ~ Eval (DropDims ds ls st)
  ) =>
  Dims ds ->
  SNats ls ->
  (Array si a -> Array si a -> Array si' b) -> Array st a -> Array st' b
diffs SNats xs f a = zips (SNats @ds) f (drops (SNats @ds) xs a) (dropBs (SNats @ds) xs a)

-- | Product two arrays using the supplied binary function.
--
-- For context, if the function is multiply, and the arrays are tensors,
-- then this can be interpreted as a [tensor product](https://en.wikipedia.org/wiki/Tensor_product).
-- The concept of a tensor product is a dense crossroad, and a complete treatment is elsewhere.  To quote the wiki article:
--
-- ... the tensor product can be extended to other categories of mathematical objects in addition to vector spaces, such as to matrices, tensors, algebras, topological vector spaces, and modules. In each such case the tensor product is characterized by a similar universal property: it is the freest bilinear operation. The general concept of a "tensor product" is captured by monoidal categories; that is, the class of all things that have a tensor product is a monoidal category.
--
-- >>> x = array [1,2,3] :: Array '[3] Int
-- >>> pretty $ expand (*) x x
-- [[1,2,3],
--  [2,4,6],
--  [3,6,9]]
--
-- Alternatively, expand can be understood as representing the permutation of element pairs of two arrays, so like the Applicative List instance.
--
-- >>> i2 = indices @[2,2]
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
  forall sc sa sb a b c.
  ( KnownNats sa,
    KnownNats sb,
    KnownNats sc,
    sc ~ Eval ((++) sa sb)
  ) =>
  (a -> b -> c) ->
  Array sa a ->
  Array sb b ->
  Array sc c
expand f a b = tabulate (\i -> f (index a (UnsafeFins $ List.take r (fromFins i))) (index b (UnsafeFins $ List.drop r (fromFins i))))
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
  forall sc sa sb a b c.
  ( KnownNats sa,
    KnownNats sb,
    KnownNats sc,
    sc ~ Eval ((++) sa sb)
  ) =>
  (a -> b -> c) ->
  Array sa a ->
  Array sb b ->
  Array sc c
expandr f a b = tabulate (\i -> f (index a (UnsafeFins $ List.drop r (fromFins i))) (index b (UnsafeFins $ List.take r (fromFins i))))
  where
    r = rank a

-- | Contract an array by applying the supplied (folding) function on diagonal elements of the dimensions.
--
-- This generalises a tensor contraction by allowing the number of contracting diagonals to be other than 2.
--
-- FIXME: relook at expand/contract structure
--
-- >>> let b = array [1..6] :: Array [2,3] Int
-- >>> pretty $ contract (S.SNats @[1,2]) sum (expand (*) b (transpose b))
-- [[14,32],
--  [32,77]]
contract ::
  forall a b s ss se s' ds ds'.
  ( KnownNats se,
    se ~ Eval (DeleteDims ds' s),
    KnownNats ds',
    KnownNats s,
    KnownNats ss,
    KnownNats s',
    s' ~ Eval (GetDims ds' s),
    ss ~ Eval (MinDim se),
    ds' ~ Eval (ExceptDims ds s)
  ) =>
  Dims ds ->
  (Array ss a -> b) ->
  Array s a ->
  Array s' b
contract SNats f a = f . diag <$> extracts (SNats @ds') a

-- | A generalisation of a dot operation, which is a multiplicative expansion of two arrays and sum contraction along the middle two dimensions.
--
-- matrix multiplication
--
-- >>> let b = array [1..6] :: Array [2,3] Int
-- >>> pretty $ dot sum (*) b (transpose b)
-- [[14,32],
--  [32,77]]
--
-- inner product
--
-- >>> let v = array [1..3] :: Array '[3] Int
-- >>> pretty $ dot sum (*) v v
-- 14
--
-- matrix-vector multiplication
-- Note that an Array with shape [3] is neither a row vector nor column vector.
--
-- >>> pretty $ dot sum (*) v b
-- [9,12,15]
--
-- >>> pretty $ dot sum (*) b v
-- [14,32]
dot ::
  forall a b c d ds ds' s sa sb s' ss se.
  ( KnownNats sa,
    KnownNats sb,
    KnownNats se,
    KnownNat (Eval (Rank sa) - 1),
    KnownNat (Eval (Rank sa)),
    KnownNats ss,
    KnownNats s',
    KnownNats ds,
    KnownNats ds',
    KnownNats s,
    s ~ Eval ((++) sa sb),
    ds ~ '[Eval ((Fcf.-) (Eval (Rank sa))  1), Eval (Rank sa)],
    ds' ~ Eval (ExceptDims ds s),
    s' ~ Eval (GetDims ds' s),
    se ~ Eval (DeleteDims ds' s),
    ss ~ Eval (MinDim se)
  ) =>
  (Array ss c -> d) ->
  (a -> b -> c) ->
  Array sa a ->
  Array sb b ->
  Array s' d
dot f g a b = contract (SNats :: SNats ds) f (expand g a b)

-- | Array multiplication.
--
-- matrix multiplication
--
-- >>> let b = array [1..6] :: Array [2,3] Int
-- >>> pretty $ mult b (transpose b)
-- [[14,32],
--  [32,77]]
--
-- inner product
--
-- >>> let v = array @'[3] [1..3::Int]
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
  forall a sa sb s s' ss se ds ds'.
  ( Additive a,
    Multiplicative a,
    KnownNats sa,
    KnownNats sb,
    KnownNats se,
    KnownNat (Eval (Rank sa) - 1),
    KnownNat (Eval (Rank sa)),
    KnownNats ss,
    KnownNats s',
    KnownNats ds,
    KnownNats ds',
    KnownNats s,
    s ~ Eval ((++) sa sb),
    ds ~ '[Eval (Rank sa) - 1, Eval (Rank sa)],
    ds' ~ Eval (ExceptDims ds s),
    s' ~ Eval (GetDims ds' s),
    se ~ Eval (DeleteDims ds' s),
    ss ~ Eval (MinDim se)
  ) =>
  Array sa a ->
  Array sb a ->
  Array s' a
mult = dot sum (*)

-- | windows xs are xs-sized windows of an array
--
-- >>> shape $ windows (S.SNats @[2,2]) (range @[4,3,2])
-- [3,2,2,2,2]
windows :: forall w s ws a.
  ( KnownNats s,
    KnownNats ws,
    ws ~ Eval (ExpandWindows w s)) =>
  SNats w -> Array s a -> Array ws a
windows SNats a = unsafeBackpermute (S.indexWindows (rankOf @w)) a

-- | Find the starting positions of occurences of one array in another.
--
-- >>> a = cycle @[4,4] (range @'[3])
-- >>> i = array @[2,2] [1,2,2,0]
-- >>> pretty $ find i a
-- [[False,True,False],
--  [True,False,False],
--  [False,False,True]]
find ::
  forall s' si s a r i' re ws.
  (Eq a,
   KnownNats si,
   KnownNats s,
   KnownNats s',
   KnownNats re,
   KnownNats i',
   KnownNat r,
   KnownNats ws,
   ws ~ Eval (ExpandWindows i' s),
   r ~ Eval (Rank s),
   i' ~ Eval (Rerank r si),
   re ~ Eval (DimWindows ws s),
   i' ~ Eval (DeleteDims re ws),
   s' ~ Eval (GetDims re ws)
  ) =>
  Array si a -> Array s a -> Array s' Bool
find i a = xs
  where
    i' = rerank (SNat @r) i
    ws = windows (SNats @i') a
    xs = fmap (== i') (extracts (SNats @re) ws)

-- | Check if the first array is a prefix of the second
--
-- FIXME: different to D.isPrefixOf result
-- >>> isPrefixOf (array @[2,2] [0,4,12,16]) a
-- True
isPrefixOf ::
  forall s' s a.
  (Eq a,
   KnownNats s,
   KnownNats s',
   True ~ Eval (IsSubset s' s)) =>
  Array s' a -> Array s a -> Bool
isPrefixOf p a = p == cut a

-- | Check if the first array is a suffix of the second
--
-- >>> isSuffixOf (array @[2,2] [18,19,22,23]) a
-- True
isSuffixOf ::
  forall s' s r a.
  (Eq a,
   KnownNats s,
   KnownNats s',
   KnownNat r,
   KnownNats (Eval (Rerank r s)),
   r ~ Eval (Rank s'),
   True ~ Eval (IsSubset s' s)) =>
  Array s' a -> Array s a -> Bool
isSuffixOf p a = p == cutSuffix a

-- | Check if the first array is an infix of the second
--
-- >>> isInfixOf (array @[2,2] [18,19,22,23]) a
-- True
isInfixOf ::
  forall s' si s a r i' re ws.
  (Eq a,
   KnownNats si,
   KnownNats s,
   KnownNats s',
   KnownNats re,
   KnownNats i',
   KnownNat r,
   KnownNats ws,
   ws ~ Eval (ExpandWindows i' s),
   r ~ Eval (Rank s),
   i' ~ Eval (Rerank r si),
   re ~ Eval (DimWindows ws s),
   i' ~ Eval (DeleteDims re ws),
   s' ~ Eval (GetDims re ws)
  ) =>
  Array si a -> Array s a -> Bool
isInfixOf p a = or $ find p a

-- | Fill an array with the supplied value without regard to the original shape or cut the array values to match array size.
--
-- > validate (def x a) == True
--
-- >>> pretty $ fill @'[3] 0 (array @'[0] [])
-- [0,0,0]
-- >>> pretty $ fill @'[3] 0 (array @'[4] [1..4])
-- [1,2,3]
fill ::
  forall s' a s.
  (KnownNats s,
   KnownNats s') =>
  a -> Array s a -> Array s' a
fill x (Array v) = Array (V.take (S.size (valuesOf @s')) (v <> V.replicate (S.size (valuesOf @s') - V.length v) x))

-- | Cut an array to form a new (smaller) shape. Errors if the new shape is larger. The old array is reranked to the rank of the new shape first.
--
-- >>> toDynamic $ cut @'[2] (array @'[4] @Int [0..3])
-- UnsafeArray [2] [0,1]
cut ::
  forall s' s a.
  (KnownNats s,
   KnownNats s',
   True ~ Eval (IsSubset s' s)) =>
  Array s a ->
  Array s' a
cut a = unsafeBackpermute id a

-- | Cut an array to form a new (smaller) shape, using suffix elements. Errors if the new shape is larger. The old array is reranked to the rank of the new shape first.
--
-- >>> toDynamic $ cutSuffix @[2,2] a
-- UnsafeArray [2,2] [18,19,22,23]
cutSuffix ::
  forall s' s a r.
  (KnownNats s,
   KnownNats s',
   KnownNat r,
   KnownNats (Eval (Rerank r s)),
   r ~ Eval (Rank s'),
   True ~ Eval (IsSubset s' s)) =>
  Array s a ->
  Array s' a
cutSuffix a = unsafeBackpermute (List.zipWith (+) diffDim) a'
  where
    a' = rerank (SNat @r) a
    diffDim = List.zipWith (-) (shape a') (valuesOf @s')

-- | Pad an array to form a new shape, supplying a default value for elements outside the shape of the old array. The old array is reranked to the rank of the new shape first.
--
-- >>> toDynamic $ pad @'[5] 0 (array @'[4] @Int [0..3])
-- UnsafeArray [5] [0,1,2,3,0]
pad ::
  forall s' a s r.
  (KnownNats s,
   KnownNats s',
   KnownNat r,
   KnownNats (Eval (Rerank r s)),
   r ~ Eval (Rank s')) =>
  a ->
  Array s a ->
  Array s' a
pad d a = tabulate (\s -> bool d (index a' (unsafeCoerce s)) ((fromFins s) `S.isFins` (shape a')))
  where
    a' = rerank (SNat @r) a

-- | Left pad an array to form a new shape, supplying a default value for elements outside the shape of the old array.
--
-- >>> toDynamic $ lpad @'[5] 0 (array @'[4] [0..3])
-- UnsafeArray [5] [0,0,1,2,3]
-- >>> pretty $ lpad @[3,3] 0 (range @[2,2])
-- [[0,0,0],
--  [0,0,1],
--  [0,2,3]]
lpad ::
  forall s' a s r.
  (KnownNats s,
   KnownNats s',
   KnownNat r,
   KnownNats (Eval (Rerank r s)),
   r ~ Eval (Rank s')) =>
  a ->
  Array s a ->
  Array s' a
lpad d a = tabulate (\s -> bool d (index a' (UnsafeFins $ olds s)) ((olds s) `S.isFins` (shape a')))
  where
    a' = rerank (SNat @r) a
    gap = List.zipWith (-) (valuesOf @s') (shape a')
    olds s = List.zipWith (-) (fromFins s) gap

-- | Reshape an array (with the same number of elements).
--
-- >>> pretty $ reshape @[4,3,2] a
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
  forall s' s a.
  ( Eval (Size s) ~ Eval (Size s'),
    KnownNats s,
    KnownNats s'
  ) =>
  Array s a ->
  Array s' a
reshape = unsafeBackpermute (shapen s . flatten s')
  where
    s = valuesOf @s
    s' = valuesOf @s'

-- | Make an Array single dimensional
--
-- >>> pretty $ flat (range @[2,2])
-- [0,1,2,3]
-- >>> pretty (flat $ toScalar 0)
-- [0]
flat :: forall s' s a. (KnownNats s, KnownNats s', s' ~ ('[Eval (Size s)])) => Array s a -> Array s' a
flat a = unsafeModifyShape a

-- | Reshape an array, repeating the original array. The shape of the array should be a suffix of the new shape.
--
-- >>> pretty $ repeat @[2,2,2] (array @'[2] [1,2])
-- [[[1,2],
--   [1,2]],
--  [[1,2],
--   [1,2]]]
--
-- > repeat ds (toScalar x) == konst ds x
repeat ::
  forall s' s a.
  (KnownNats s,
   KnownNats s',
   Eval (IsPrefixOf s s') ~ True) =>
  Array s a ->
  Array s' a
repeat a = unsafeBackpermute (List.drop (S.rank (valuesOf @s') - rank a)) a

-- | Reshape an array, cycling through the elements without regard to the original shape.
--
-- >>> pretty $ cycle @[2,2,2] (array @'[3] [1,2,3])
-- [[[1,2],
--   [3,1]],
--  [[2,3],
--   [1,2]]]
cycle ::
  forall s' s a.
  (KnownNats s,
   KnownNats s') =>
  Array s a ->
  Array s' a
cycle a = unsafeBackpermute (S.shapen (shape a) . (`mod` (size a)) . S.flatten (valuesOf @s')) a

-- | Change rank by adding new dimensions at the front, if the new rank is greater, or combining dimensions (from left to right) into rows, if the new rank is lower.
--
-- >>> shape (rerank (SNat @4) a)
-- [1,2,3,4]
-- >>> shape (rerank (SNat @2) a)
-- [6,4]
--
-- > flat == rerank 1
rerank ::
  forall r s s' a.
  (KnownNats s,
   KnownNats s',
   s' ~ Eval (Rerank r s)) =>
  SNat r -> Array s a -> Array s' a
rerank _ a = unsafeModifyShape a

-- | Change the order of dimensions.
--
-- >>> pretty $ reorder (S.SNats @[2,0,1]) a
-- [[[0,4,8],
--   [12,16,20]],
--  [[1,5,9],
--   [13,17,21]],
--  [[2,6,10],
--   [14,18,22]],
--  [[3,7,11],
--   [15,19,23]]]
reorder ::
  forall ds s s' a.
  (KnownNats s,
   KnownNats s',
   s' ~ Eval (Reorder s ds)
  ) =>
  SNats ds ->
  Array s a ->
  Array s' a
reorder SNats a = unsafeBackpermute (\s -> S.insertDims (valuesOf @ds) s []) a

-- | Remove single dimensions.
--
-- >>> let sq = array [1..24] :: Array '[2,1,3,4,1] Int
-- >>> pretty sq
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
-- >>> pretty $ squeeze sq
-- [[[1,2,3,4],
--   [5,6,7,8],
--   [9,10,11,12]],
--  [[13,14,15,16],
--   [17,18,19,20],
--   [21,22,23,24]]]
--
-- >>> pretty $ squeeze (array [1] :: Array '[1,1] Double)
-- 1.0
squeeze ::
  forall s t a.
  (KnownNats s,
   KnownNats t,
   t ~ Eval (Squeeze s)) =>
  Array s a ->
  Array t a
squeeze = unsafeModifyShape

-- | Insert a single dimension at the supplied position.
--
-- >>> shape $ elongate (SNat @1) a
-- [2,1,3,4]
-- >>> toDynamic $ elongate (SNat @0) (toScalar 1)
-- UnsafeArray [1] [1]
elongate ::
  (KnownNats s,
   KnownNats s',
   s' ~ Eval (InsertDim d 1 s)) =>
  Dim d ->
  Array s a ->
  Array s' a
elongate _ a = unsafeModifyShape a

-- | Reverse indices eg transposes the element A/ijk/ to A/kji/.
--
-- >>> (transpose a) ! [1,0,0] == a ! [0,0,1]
-- True
-- >>> pretty $ transpose (array @[2,2,2] [1..8])
-- [[[1,5],
--   [3,7]],
--  [[2,6],
--   [4,8]]]
transpose ::
  forall a s s'. (KnownNats s, KnownNats s', s' ~ Eval (Reverse s)) => Array s a -> Array s' a
transpose a = unsafeBackpermute List.reverse a

-- | Inflate an array by inserting a new dimension given a supplied dimension and size.
--
-- alt name: replicate
--
-- >>> pretty $ inflate (SNat @0) (SNat @2) (array @'[3] [0,1,2])
-- [[0,1,2],
--  [0,1,2]]
inflate ::
  forall s' s d x a.
  (KnownNats s,
   KnownNats s',
   s' ~ Eval (InsertDim d x s)) =>
  Dim d ->
  SNat x ->
  Array s a ->
  Array s' a
inflate SNat _ a = unsafeBackpermute (S.deleteDim (valueOf @d)) a

-- | Intercalate an array along dimensions.
--
-- >>> pretty $ intercalate @2 (konst @[2,3] 0) a
-- [[[0,0,1,0,2,0,3],
--   [4,0,5,0,6,0,7],
--   [8,0,9,0,10,0,11]],
--  [[12,0,13,0,14,0,15],
--   [16,0,17,0,18,0,19],
--   [20,0,21,0,22,0,23]]]
intercalate::
  forall d ds n n' s si st a.
  ( KnownNats s
  , KnownNats si
  , KnownNats st
  , KnownNats ds
  , KnownNat n
  , KnownNat n'
  , ds ~ '[d]
  , si ~ Eval (DeleteDim d s)
  , n ~ Eval (GetDim d s)
  , n' ~ n + n - 1
  , st ~ Eval (InsertDim d n' si)
  ) =>
  Dim d -> Array s a -> Array si a -> Array st a
intercalate SNat a i =
  joins (SNats @ds)
  (vector @n'
  (List.intersperse i
  (toList (extracts (SNats @ds) a :: Array '[n] (Array si a)))))

-- | Concatenate dimensions, creating a new dimension at the supplied postion.
--
-- >>> pretty $ concats (S.SNats @[0,1]) (SNat @1) a
-- [[0,4,8,12,16,20],
--  [1,5,9,13,17,21],
--  [2,6,10,14,18,22],
--  [3,7,11,15,19,23]]
concats ::
  forall s s' newd ds a.
  (KnownNats s,
   KnownNats s',
   s' ~ Eval (ConcatDims ds newd s)) =>
  Dims ds ->
  SNat newd ->
  Array s a ->
  Array s' a
concats SNats SNat a = unsafeBackpermute (unconcatDimsIndex ds n (shape a)) a
  where
    n = valueOf @newd
    ds = valuesOf @ds

-- | Reverses element order along specified dimensions.
--
-- >>> pretty $ reverses (S.SNats @[0,1]) a
-- [[[20,21,22,23],
--   [16,17,18,19],
--   [12,13,14,15]],
--  [[8,9,10,11],
--   [4,5,6,7],
--   [0,1,2,3]]]
reverses ::
  forall ds s a.
  (KnownNats s) =>
  Dims ds ->
  Array s a ->
  Array s a
reverses SNats a = unsafeBackpermute (reverseIndex (valuesOf @ds) (shape a)) a

-- | Rotate an array by/along dimensions & offsets.
--
-- >>> pretty $ rotates (S.SNats @'[1]) [2] a
-- [[[8,9,10,11],
--   [0,1,2,3],
--   [4,5,6,7]],
--  [[20,21,22,23],
--   [12,13,14,15],
--   [16,17,18,19]]]
rotates ::
  forall a ds s.
  (KnownNats s,
   True ~ Eval (IsDims ds s)) =>
  Dims ds ->
  [Int] ->
  Array s a ->
  Array s a
rotates SNats rs a = unsafeBackpermute (rotatesIndex (valuesOf @ds) rs (valuesOf @s)) a

-- | Sort an array along the supplied dimensions.
--
-- >>> pretty $ sorts (S.SNats @'[0]) (array @[2,2] [2,3,1,4])
-- [[1,4],
--  [2,3]]
-- >>> pretty $ sorts (S.SNats @'[1]) (array @[2,2] [2,3,1,4])
-- [[2,3],
--  [1,4]]
-- >>> pretty $ sorts (S.SNats @[0,1]) (array @[2,2] [2,3,1,4])
-- [[1,2],
--  [3,4]]
sorts ::
  forall ds s a si so.
  (Ord a,
   KnownNats s,
   KnownNats si,
   KnownNats so,
   si ~ Eval (DeleteDims ds s),
   so ~ Eval (GetDims ds s),
   s ~ Eval (InsertDims ds so si)
  ) =>
  Dims ds -> Array s a -> Array s a
sorts SNats a = joins (SNats @ds) $ unsafeModifyVector sortV (extracts (SNats @ds) a)

-- | The indices into the array if it were sorted by a comparison function along the dimensions supplied.
--
-- >>> import Data.Ord (Down (..))
-- >>> toDynamic $ sortsBy (S.SNats @'[0]) (fmap Down) (array @[2,2] [2,3,1,4])
-- UnsafeArray [2,2] [2,3,1,4]
sortsBy ::
  forall ds s a b si so.
  (Ord b,
   KnownNats s,
   KnownNats si,
   KnownNats so,
   si ~ Eval (DeleteDims ds s),
   so ~ Eval (GetDims ds s),
   s ~ Eval (InsertDims ds so si)
  ) =>
  Dims ds -> (Array si a -> Array si b) -> Array s a -> Array s a
sortsBy SNats c a = joins (SNats @ds) $ unsafeModifyVector (sortByV c) (extracts (SNats @ds) a)

-- | The indices into the array if it were sorted along the dimensions supplied.
--
-- >>> orders (S.SNats @'[0]) (array @[2,2] [2,3,1,4])
-- [1,0]
orders ::
  forall ds s a si so.
  (Ord a,
   KnownNats s,
   KnownNats si,
   KnownNats so,
   si ~ Eval (DeleteDims ds s),
   so ~ Eval (GetDims ds s),
   s ~ Eval (InsertDims ds so si)
  ) =>
  Dims ds -> Array s a -> Array so Int
orders SNats a = unsafeModifyVector orderV (extracts (SNats @ds) a)

-- | The indices into the array if it were sorted by a comparison function along the dimensions supplied.
--
-- >>> import Data.Ord (Down (..))
-- >>> ordersBy (S.SNats @'[0]) (fmap Down) (array @[2,2] [2,3,1,4])
-- [0,1]
ordersBy ::
  forall ds s a b si so.
  (Ord b,
   KnownNats s,
   KnownNats si,
   KnownNats so,
   si ~ Eval (DeleteDims ds s),
   so ~ Eval (GetDims ds s),
   s ~ Eval (InsertDims ds so si)
  ) =>
  Dims ds -> (Array si a -> Array si b) -> Array s a -> Array so Int
ordersBy SNats c a = unsafeModifyVector (orderByV c) (extracts (SNats @ds) a)

-- | Apply a binary array function to two arrays with matching shapes across the supplied (matching) dimensions.
--
-- >>> a = array @[2,3] [0..5]
-- >>> b = array @'[3] [6..8]
-- >>> pretty $ telecasts (S.SNats @'[1]) (S.SNats @'[0]) (concatenate (SNat @0)) a b
-- [[0,3,6],
--  [1,4,7],
--  [2,5,8]]
telecasts ::
  forall sa sb sc sia sib sic ma mb a b c soa sob ds.
  (KnownNats sa,
   KnownNats sb,
   KnownNats sc,
   KnownNats sia,
   KnownNats sib,
   KnownNats sic,
   KnownNats soa,
   KnownNats sob,
   KnownNats ds,
   ds ~ Eval (DimsOf soa),
   sia ~ Eval (DeleteDims ma sa),
   sib ~ Eval (DeleteDims mb sb),
   soa ~ Eval (GetDims ma sa),
   sob ~ Eval (GetDims mb sb),
   soa ~ sob,
   sc ~ Eval (InsertDims ds soa sic)
  ) =>
  SNats ma -> SNats mb -> (Array sia a -> Array sib b -> Array sic c) -> Array sa a -> Array sb b -> Array sc c
telecasts SNats SNats f a b = join (zipWith f (extracts (SNats @ma) a) (extracts (SNats @mb) b))

-- | Apply a binary array function to two arrays where the shape of the first array is a prefix of the second array.
--
-- >>> a = array @[2,3] [0..5]
-- >>> pretty $ transmit (zipWith (+)) (toScalar 1) a
-- [[1,2,3],
--  [4,5,6]]
--
transmit ::
  forall sa sb sc a b c ds sib sic sob.
  (KnownNats sa,
   KnownNats sb,
   KnownNats sc,
   KnownNats ds,
   KnownNats sib,
   KnownNats sic,
   KnownNats sob,
   ds ~ Eval (EnumFromTo (Eval (Rank sa)) (Eval (Rank sb) - 1)),
   sib ~ Eval (DeleteDims ds sb),
   sob ~ Eval (GetDims ds sb),
   sb ~ Eval (InsertDims ds sob sib),
   sc ~ Eval (InsertDims ds sob sic),
   True ~ (Eval (IsPrefixOf sa sb))) =>
  (Array sa a -> Array sib b -> Array sic c) -> Array sa a -> Array sb b -> Array sc c
transmit f a b = maps (SNats @ds) (f a) b

-- | <https://en.wikipedia.org/wiki/Vector_(mathematics_and_physics) Wiki Vector>
type Vector s a = Array '[s] a

-- | A one-dimensional array.
--
-- >>> pretty $ vector @3 @Int [2,3,4]
-- [2,3,4]
vector ::
  forall n a t.
  (FromVector t a,
   KnownNat n) =>
  t ->
  Array '[n] a
vector xs = array xs

-- | vector with an explicit SNat rather than a KnownNat constraint.
--
-- >>> pretty $ vector @3 @Int [2,3,4]
-- [2,3,4]
vector' ::
  forall n a t.
  (FromVector t a) =>
  SNat n ->
  t ->
  Array '[n] a
vector' n xs = withKnownNat n (vector xs)

-- | Vector specialisation of 'range'
--
-- >>> toDynamic $ iota @5
-- UnsafeArray [5] [0,1,2,3,4]
iota :: forall n. (KnownNat n) => Vector n Int
iota = range

-- | <https://en.wikipedia.org/wiki/Matrix_(mathematics) Wiki Matrix>
type Matrix m n a = Array '[m, n] a

instance
  ( Multiplicative a,
    P.Distributive a,
    Subtractive a,
    KnownNat m
  ) =>
  Multiplicative (Matrix m m a)
  where
  (*) = mult

  one = ident

instance
  ( Multiplicative a,
    P.Distributive a,
    Subtractive a,
    Eq a,
    ExpField a,
    KnownNat m
  ) =>
  Divisive (Matrix m m a)
  where
  recip a = invtri (transpose (chol a)) * invtri (chol a)

-- * row (first dimension) specializations

-- | Add a new row
--
-- >>> pretty $ cons (array @'[2] [0,1]) (array @[2,2] [2,3,4,5])
-- [[0,1],
--  [2,3],
--  [4,5]]
cons ::
  forall st s sh a.
  ( KnownNats st,
    KnownNats s,
    KnownNats sh,
    True ~ Eval (InsertOk 0 st sh),
    s ~ Eval (IncAt 0 st),
    sh ~ Eval (DeleteDim 0 st)
  ) =>
  Array sh a -> Array st a -> Array s a
cons =
  prepend (SNat @0)

-- | Add a new row at the end
--
-- >>> pretty $ snoc (array @[2,2] [0,1,2,3]) (array @'[2] [4,5])
-- [[0,1],
--  [2,3],
--  [4,5]]
snoc :: forall si s sl a.
  ( KnownNats si,
    KnownNats s,
    KnownNats sl,
    True ~ Eval (InsertOk 0 si sl),
    s ~ Eval (IncAt 0 si),
    sl ~ Eval (DeleteDim 0 si)
  ) =>
  Array si a -> Array sl a -> Array s a
snoc = append (SNat @0)

-- | split an array into the first row and the remaining rows.
--
-- >>> import Data.Bifunctor (bimap)
-- >>> bimap toDynamic toDynamic $ uncons (array @[3,2] [0..5])
-- (UnsafeArray [2] [0,1],UnsafeArray [2,2] [2,3,4,5])
uncons ::
  forall a s sh st ls os ds.
  (KnownNats s,
   KnownNats sh,
   KnownNats st,
   ds ~ '[0],
   sh ~ Eval (DeleteDims ds s),
   KnownNats ls,
   KnownNats os,
   os ~ Eval (Replicate (Eval (Rank ds)) 1),
   ls ~ Eval (GetLastPositions ds s),
   Eval (SlicesOk ds os ls s) ~ True,
   st ~ Eval (SetDims ds ls s)
  ) =>
  Array s a -> (Array sh a, Array st a)
uncons a = (heads (SNats @ds) a, tails (SNats @ds) a)

-- | split an array into the initial rows and the last row.
--
-- >>> import Data.Bifunctor (bimap)
-- >>> bimap toDynamic toDynamic $ unsnoc (array @[3,2] [0..5])
-- (UnsafeArray [2,2] [0,1,2,3],UnsafeArray [2] [4,5])
unsnoc ::
  forall ds os s a ls si sl.
  ( KnownNats s,
    KnownNats ds,
    KnownNats si,
    KnownNats ls,
    KnownNats os,
    KnownNats sl,
    ds ~ '[0],
    Eval (SlicesOk ds os ls s) ~ True,
    os ~ Eval (Replicate (Eval (Rank ds)) 0),
    ls ~ Eval (GetLastPositions ds s),
    si ~ Eval (SetDims ds ls s),
    sl ~ Eval (DeleteDims ds s)
  ) => Array s a -> (Array si a, Array sl a)
unsnoc a = (inits (SNats @ds) a, lasts (SNats @ds) a)

-- | Convenience pattern for row extraction and consolidation at the beginning of an Array.
--
-- >>> (x:<xs) = array @'[4] [0..3]
-- >>> toDynamic x
-- UnsafeArray [] [0]
-- >>> toDynamic xs
-- UnsafeArray [3] [1,2,3]
-- >>> toDynamic (x:<xs)
-- UnsafeArray [4] [0,1,2,3]
pattern (:<) ::
  forall s sh st a os ls ds.
  (KnownNats s,
   KnownNats sh,
   KnownNats st,
   True ~ Eval (InsertOk 0 st sh),
   s ~ Eval (IncAt 0 st),
   ds ~ '[0],
   sh ~ Eval (DeleteDims ds s),
   KnownNats ls,
   KnownNats os,
   Eval (SlicesOk ds os ls s) ~ True,
   os ~ Eval (Replicate (Eval (Rank ds)) 1),
   ls ~ Eval (GetLastPositions ds s),
   st ~ Eval (SetDims ds ls s)) =>
  Array sh a -> Array st a -> Array s a
pattern x :< xs <- (uncons -> (x, xs))
  where
    x :< xs = cons x xs

infix 5 :<

{-# COMPLETE (:<) :: Array #-}

-- | Convenience pattern for row extraction and consolidation at the end of an Array.
--
-- >>> (xs:>x) = array @'[4] [0..3]
-- >>> toDynamic x
-- UnsafeArray [] [3]
-- >>> toDynamic xs
-- UnsafeArray [3] [0,1,2]
-- >>> toDynamic (xs:>x)
-- UnsafeArray [4] [0,1,2,3]
pattern (:>) ::
  forall si sl s a ds ls os.
  (KnownNats si,
   KnownNats sl,
   KnownNats s,
   True ~ Eval (InsertOk 0 si sl),
   s ~ Eval (IncAt 0 si),
   KnownNats ds,
   KnownNats ls,
   KnownNats os,
   sl ~ Eval (DeleteDim 0 si),
   ds ~ '[0],
   Eval (SlicesOk ds os ls s) ~ True,
   os ~ Eval (Replicate (Eval (Rank ds)) 0),
   ls ~ Eval (GetLastPositions ds s),
   si ~ Eval (SetDims ds ls s),
   sl ~ Eval (DeleteDims ds s)
   ) =>
  Array si a -> Array sl a -> Array s a
pattern xs :> x <- (unsnoc -> (xs, x))
  where
    xs :> x = snoc xs x

infix 5 :>

{-# COMPLETE (:>) :: Array #-}

-- | GENERATE an array of uniform random variates between a range.
--
-- >>> import System.Random.Stateful hiding (uniform)
-- >>> g <- newIOGenM (mkStdGen 42)
-- >>> u <- uniform @[2,3,4] @Int g (0,9)
-- >>> pretty u
-- [[[0,7,0,2],
--   [1,7,4,2],
--   [5,9,8,2]],
--  [[9,8,1,0],
--   [2,2,8,2],
--   [2,8,0,6]]]
uniform ::
  forall s a g m.
  ( StatefulGen g m,
    UniformRange a,
    KnownNats s) => g -> (a,a) -> m (Array s a)
uniform g r = do
  v <- V.replicateM (S.size (valuesOf @s)) (uniformRM r g)
  pure $ array v

-- | Inverse of a square matrix.
--
-- > D.mult (D.inverse a) a == a
--
-- >>> e = array @[3,3] @Double [4,12,-16,12,37,-43,-16,-43,98]
-- >>> pretty (inverse e)
-- [[49.36111111111111,-13.555555555555554,2.1111111111111107],
--  [-13.555555555555554,3.7777777777777772,-0.5555555555555555],
--  [2.1111111111111107,-0.5555555555555555,0.1111111111111111]]
--
inverse :: (Eq a, ExpField a, KnownNat m) => Matrix m m a -> Matrix m m a
inverse a = mult (invtri (transpose (chol a))) (invtri (chol a))

-- | [Inversion of a Triangular Matrix](https://math.stackexchange.com/questions/1003801/inverse-of-an-invertible-upper-triangular-matrix-of-order-3)
--
-- >>> t = array @[3,3] @Double [1,0,1,0,1,2,0,0,1]
-- >>> pretty (invtri t)
-- [[1.0,0.0,-1.0],
--  [0.0,1.0,-2.0],
--  [0.0,0.0,1.0]]
-- >>> ident == mult t (invtri t)
-- True
invtri :: forall a n. (KnownNat n, ExpField a, Eq a) => Matrix n n a -> Matrix n n a
invtri a = sum (fmap (l ^) (iota @n)) * ti
  where
    ti = undiag (fmap recip (diag a))
    tl = a - undiag (diag a)
    l = negate (ti * tl)

-- | cholesky decomposition
--
-- Uses the <https://en.wikipedia.org/wiki/Cholesky_decomposition#The_Cholesky_algorithm Cholesky-Crout> algorithm.
--
-- >>> e = array @[3,3] @Double [4,12,-16,12,37,-43,-16,-43,98]
-- >>> pretty (chol e)
-- [[2.0,0.0,0.0],
--  [6.0,1.0,0.0],
--  [-8.0,5.0,3.0]]
-- >>> mult (chol e) (transpose (chol e)) == e
-- True
chol :: (KnownNat m, ExpField a) => Matrix m m a -> Matrix m m a
chol a =
  let l =
        unsafeTabulate
          ( \[i, j] ->
              bool
                ( one
                    / unsafeIndex l [j, j]
                    * ( unsafeIndex a [i, j]
                          - sum
                            ( (\k -> unsafeIndex l [i, k] * unsafeIndex l [j, k])
                                <$> ([zero .. (j - one)] :: [Int])
                            )
                      )
                )
                ( sqrt
                    ( unsafeIndex a [i, i]
                        - sum
                          ( (\k -> unsafeIndex l [j, k] ^ 2)
                              <$> ([zero .. (j - one)] :: [Int])
                          )
                    )
                )
                (i == j)
          )
   in l

