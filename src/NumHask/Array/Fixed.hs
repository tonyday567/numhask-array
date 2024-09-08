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
    concatenate,
    insert,
    delete,
    append,
    prepend,
    couple,
    slice,

    -- * Operators
    takes,
    takeBs,
    drops,
    dropBs,
    indexes,
    indexesT,
    indexesExcept,
    heads,
    lasts,
    tails,
    inits,
    slices,

    -- * Function application
    extracts,
    extractsExcept,
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

    -- * Maths
    uniform,
    invtri,
    inverse,
    chol,

    -- * Shape specializations
    Vector,
    vector,
    vector',
    SomeVector (..),
    withLength,
    aVector,
    example_insert,
    example_append,
    SomeVector' (..),
    someVector',
    aVector',
    example_insert',
    example_append',
    iota,
    Matrix,
)
where

import Data.Distributive (Distributive (..))
import Data.Functor.Classes
import Data.Functor.Rep
import Data.Proxy
import Data.Vector qualified as V
import Fcf hiding (type (&&), type (+), type (-), type (++))
import Fcf qualified
import Fcf.Data.List
import GHC.TypeNats
import NumHask.Array.Dynamic qualified as D
import NumHask.Array.Shape hiding (concatenate, rank, size, asScalar, asSingleton, squeeze, rotate, reorder, rerank)
import NumHask.Array.Shape qualified as S
import NumHask.Prelude as P hiding (Min, take, drop, diff, zipWith, empty, sequence, toList, length, repeat, cycle, find)
import Prelude qualified
import Prettyprinter hiding (dot, fill)
import Data.List qualified as List
import System.Random hiding (uniform)
import System.Random.Stateful hiding (uniform)
import Unsafe.Coerce
import NumHask.Array.Sort
-- import Data.Reflection
-- import Unsafe.Coerce
import Type.Reflection
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
-- >>> import GHC.TypeNats
-- >>> import Data.Proxy
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
  type Rep (Array s) = Fins s

  tabulate f =
    Array . V.generate (S.size s) $ (f . UnsafeFins . shapen s)
    where
      s = shapeOf @s
  {-# INLINE tabulate #-}

  index (Array v) i = V.unsafeIndex v (flatten s (fromFins i))
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
unsafeArray :: (HasShape s, FromVector t a) => t -> Array s a
unsafeArray (asVector -> v) = Array v

-- | Validate the size and shape of an array.
--
-- >>> validate (unsafeArray [0..4] :: Array [2,3] Int)
-- False
validate :: (HasShape s) => Array s a -> Bool
validate a = size a == V.length (asVector a)

-- | Construct an Array, checking shape.
--
-- >>> (safeArray [0..23] :: Maybe (Array [2,3,4] Int)) == Just a
-- True
safeArray :: (HasShape s, FromVector t a) => t -> Maybe (Array s a)
safeArray v =
  bool Nothing (Just a) (validate a)
  where
    a = unsafeArray v

-- | Construct an Array, checking shape.
--
-- >>> array [0..22] :: Array [2,3,4] Int
-- *** Exception: NumHaskException {errorMessage = "ShapeMismatch"}
array :: forall s a t. (HasShape s, FromVector t a) => t -> Array s a
array v =
  fromMaybe (throw (NumHaskException "ShapeMismatch")) (safeArray v)

-- | Unsafely modify an array shape.
--
-- >>> pretty (unsafeModifyShape (array [0..5] :: Array [2,3] Int) :: Array [3,2] Int)
-- [[0,1],
--  [2,3],
--  [4,5]]
unsafeModifyShape :: (HasShape s, HasShape s') => Array s a -> Array s' a
unsafeModifyShape a = unsafeArray (asVector a)

-- | Unsafely modify an array vector.
--
-- >>> import Data.Vector qualified as V
-- >>> pretty (unsafeModifyVector (V.map (+1)) (array [0..5] :: Array [2,3] Int))
-- [[1,2,3],
--  [4,5,6]]
unsafeModifyVector :: (HasShape s) => (FromVector u a) => (FromVector v b) => (u -> v) -> Array s a -> Array s b
unsafeModifyVector f a = unsafeArray (asVector (f (vectorAs (asVector a))))

-- | convert to a dynamic array with shape at the value level.
--
-- >>> toDynamic a
-- UnsafeArray [2,3,4] [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]
toDynamic :: (HasShape s) => Array s a -> D.Array a
toDynamic a = D.array (shape a) (asVector a)

-- | Use a dynamic array in a fixed context.
--
-- >>> import qualified NumHask.Array.Dynamic as D
-- >>> with (D.range [2,3,4]) (F.indexes (Proxy :: Proxy [0,1]) [1,1] :: F.Array [2,3,4] Int -> F.Array '[4] Int)
-- [16,17,18,19]
with ::
  forall a r s.
  (HasShape s) =>
  D.Array a ->
  (Array s a -> r) ->
  r
with (D.UnsafeArray _ v) f = f (Array v)

-- | Get shape of an Array as a value.
--
-- >>> shape a
-- [2,3,4]
shape :: forall a s. (HasShape s) => Array s a -> [Int]
shape _ = shapeOf @s
{-# INLINE shape #-}

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

-- | Number of rows (first dimension size) in an Array. As a convention, a scalar value is still a single row.
--
-- >>> F.length a
-- 2
length :: (HasShape s) => Array s a -> Int
length a = case shape a of
  [] -> one
  (x : _) -> x

-- | Is the Array empty (has zero number of elements).
--
-- >>> isNull (array [] :: Array [1,0] ())
-- True
-- >>> isNull (array [4] :: Array '[] Int)
-- False
isNull :: (HasShape s) => Array s a -> Bool
isNull = (zero ==) . size

-- | Extract an element at an index, unsafely.
--
-- >>> unsafeIndex a [1,2,3]
-- 23
unsafeIndex :: (HasShape s) => Array s a -> [Int] -> a
unsafeIndex a xs = index a (UnsafeFins xs)

-- | Extract an element at an index, unsafely.
--
-- >>> a ! [1,2,3]
-- 23
(!) :: (HasShape s) => Array s a -> [Int] -> a
(!) a xs = index a (UnsafeFins xs)


-- | Extract an element at an index, safely.
--
-- >>> a !? [1,2,3]
-- Just 23
-- >>> a !? [2,3,1]
-- Nothing
(!?) :: (HasShape s) => Array s a -> [Int] -> Maybe a
(!?) a xs = index a <$> toFins xs

-- | Tabulate unsafely
unsafeTabulate :: (HasShape s) => ([Int] -> a) -> Array s a
unsafeTabulate f = tabulate (f . fromFins)

-- | Safe backpermute
backpermute :: (HasShape s, HasShape s') => (Fins s' -> Fins s) -> Array s a -> Array s' a
backpermute f a = tabulate (index a . f)
{-# INLINEABLE backpermute #-}

{- RULES
   "backpermute/backpermute" forall f f' (a :: forall a. Array a)). backpermute f (backpermute f' a) == backpermute (f . f') a
-}

-- | Unsafe backpermute
unsafeBackpermute :: (HasShape s, HasShape s') => ([Int] -> [Int]) -> Array s a -> Array s' a
unsafeBackpermute f a = tabulate (index a . UnsafeFins . f . fromFins)

{- RULES
   "unsafeBackpermute/unsafeBackpermute" forall f f' (a :: forall a. Array a)). unsafeBackpermute f (unsafeBackpermute f' a) == unsafeBackpermute (f . f') a
-}

-- | Unwrapping scalars is probably a performance bottleneck.
--
-- >>> s = array @'[] @Int [3]
-- >>> :t fromScalar s
-- fromScalar s :: Int
fromScalar :: (HasShape ('[] :: [Nat])) => Array ('[] :: [Nat]) a -> a
fromScalar a = index a (UnsafeFins [])

-- | Convert a number to a scalar.
--
-- >>> :t toScalar @Int 2
-- toScalar @Int 2 :: Array '[] Int
toScalar :: a -> Array ('[] :: [Nat]) a
toScalar a = Array (V.singleton a)

-- | Is the Array a Scalar?
--
-- >>> isScalar (toScalar (2::Int))
-- True
isScalar :: (HasShape s) => Array s a -> Bool
isScalar a = rank a == zero

-- | Convert scalars to dimensioned arrays.
--
-- >>> asSingleton (toScalar 4)
-- [4]
asSingleton :: (HasShape s, HasShape s', s' ~ Eval (AsSingleton s)) => Array s a -> Array s' a
asSingleton = unsafeModifyShape

-- | Convert arrays with shape [1] to scalars.
--
-- >>> pretty (asScalar (singleton 3))
-- 3
asScalar :: (HasShape s, HasShape s', s' ~ Eval (AsScalar s)) => Array s a -> Array s' a
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
range :: forall s. (HasShape s) => Array s Int
range = tabulate (S.flatten (shapeOf @s) . fromFins)

-- | Indices of an array shape.
--
-- >>> pretty $ indices @[3,3]
-- [[[0,0],[0,1],[0,2]],
--  [[1,0],[1,1],[1,2]],
--  [[2,0],[2,1],[2,2]]]
indices :: (HasShape s) => Array s [Int]
indices = tabulate fromFins

-- | The identity array.
--
-- >>> pretty $ ident @[3,3]
-- [[1,0,0],
--  [0,1,0],
--  [0,0,1]]
ident :: (HasShape s, Ring a) => Array s a
ident = tabulate (bool zero one . S.isDiag . fromFins)

-- | Create an array composed of a single value.
--
-- >>> pretty $ konst @[3,2] one
-- [[1,1],
--  [1,1],
--  [1,1]]
konst :: (HasShape s) => a -> Array s a
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
  ( HasShape s,
    HasShape s',
    s' ~ '[Eval (Minimum s)]
  ) =>
  Array s a ->
  Array s' a
diag a = tabulate (go . fromFins)
  where
    go [] = index a (UnsafeFins [])
    go (s' : _) = index a (UnsafeFins $ replicate (length a) s')

-- | Expand the array to form a diagonal array
--
-- >>> pretty $ undiag (range @'[3])
-- [[0,0,0],
--  [0,1,0],
--  [0,0,2]]
undiag ::
  forall s' a s.
  ( HasShape s,
    HasShape s',
    s' ~ Eval ((++) s s),
    Additive a
  ) =>
  Array s a ->
  Array s' a
undiag a = tabulate (go . fromFins)
  where
    go [] = index a (UnsafeFins [])
    go xs@(x : xs') = bool zero (index a (UnsafeFins xs)) (all (x ==) xs')

-- | Zip two arrays at an element level. Could also be called liftS2 or sometink like that.
--
-- > zipWith == \f a b -> zips (range (rank a)) (\f a b -> f (D.toScalar a) (D.toScalar b))
--
-- >>> zipWith (-) v v
-- [0,0,0]
zipWith :: (HasShape s) => (a -> b -> c) -> Array s a -> Array s b -> Array s c
zipWith f (asVector -> a) (asVector -> b) = unsafeArray (V.zipWith f a b)

-- | Modify a single value at an index.
--
-- >>> pretty $ modify (S.UnsafeFins [0,0]) (const 100) (range @[3,2])
-- [[100,1],
--  [2,3],
--  [4,5]]
modify :: (HasShape s) => Fins s -> (a -> a) -> Array s a -> Array s a
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
  (HasShape s) =>
  ([Int] -> a -> b) ->
  Array s a ->
  Array s b
imap f a = zipWith f indices a

-- | Apply a function that takes a (dimension,parameter) list and applies a parameter list to the initial dimensions. ie
--
-- > rowWise f xs = f [0..rank xs - 1] xs
--
-- >>> toDynamic $ rowWise indexesT (Proxy :: Proxy [1,0]) a
-- UnsafeArray [4] [12,13,14,15]
rowWise ::
  forall a ds s s' xs ts.
  ( HasShape s
  , HasShape ds
  , HasShape xs
  , ds ~ Eval (Range (Eval (Rank xs)))
  , ts ~ Eval (Zip ds xs)) =>
  (Proxy ts -> Array s a -> Array s' a) ->
  Proxy xs -> Array s a -> Array s' a
rowWise f _ a = f (Proxy :: Proxy ts) a

-- | Apply a function that takes a (dimension,parameter) list and applies a parameter list to the the last dimensions (in reverse). ie
--
-- > colWise f xs = f (List.reverse [0 .. (rank a - 1)]) xs
--
-- >>> toDynamic $ colWise indexesT (Proxy :: Proxy [1,0]) a
-- UnsafeArray [2] [1,13]
colWise ::
  forall a ds s s' xs ts.
  ( HasShape s
  , HasShape ds
  , HasShape xs
  , ts ~ Eval (Zip ds xs)
  -- , ds ~ Eval (Map ((Fcf.-) (Eval ((Fcf.-) (Eval (Rank s)) 1))) (Eval (Range (Eval (Rank xs)))))
  , ds ~ Eval (Take (Eval (Rank xs)) (Eval (Reverse (Eval (Range (Eval (Rank s)))))))) =>
  (Proxy ts -> Array s a -> Array s' a) ->
  Proxy xs -> Array s a -> Array s' a
colWise f _ a = f (Proxy :: Proxy ts) a

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
  forall s s' a d t.
  ( HasShape s,
    HasShape s',
    Eval (IsFin d (Eval (Rank s))) ~ True,
    Eval (SetIndex d (Eval (Min t (Eval (UnsafeGetIndex d s)))) s) ~ s'
  ) =>
  SNat d ->
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
  ( HasShape s,
    HasShape s',
    Eval (IsFin d (Eval (Rank s))) ~ True,
    Eval (SetIndex d (Eval (Min t (Eval (UnsafeGetIndex d s)))) s) ~ s'
  ) =>
  SNat d ->
  SNat t ->
  Array s a ->
  Array s' a
takeB d t a = unsafeBackpermute (\s -> modifyDim (int d) (\x -> x + (unsafeGetIndex (int d) (shape a)) - (int t)) s) a

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
  ( HasShape s,
    HasShape s',
    Eval (IsFin d (Eval (Rank s))) ~ True,
    Eval (SetIndex d (Eval ((Fcf.-) (Eval (UnsafeGetIndex d s)) t)) s) ~ s'
  ) =>
  SNat d ->
  SNat t ->
  Array s a ->
  Array s' a
drop d t a = unsafeBackpermute (S.modifyDim (int d) (\x -> x + bool (int t) 0 ((int t) < 0))) a

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
  ( HasShape s,
    HasShape s',
    Eval (IsFin d (Eval (Rank s))) ~ True,
    Eval (SetIndex d (Eval ((Fcf.-) (Eval (UnsafeGetIndex d s)) t)) s) ~ s'
  ) =>
  SNat d ->
  SNat t ->
  Array s a ->
  Array s' a
dropB _ _ a = unsafeBackpermute id a

-- | Select an index along a dimension.
--
-- >>> let s = select (SNat @2) (SNat @3) a
-- >>> pretty s
-- [[3,7,11],
--  [15,19,23]]
select ::
  forall d x a s s'.
  (HasShape s,
   HasShape s',
   s' ~ Eval (DeleteDim d s)) =>
  SNat d ->
  SNat x ->
  Array s a ->
  Array  s' a
select d x a = unsafeBackpermute (S.insertDim (int d) (int x)) a

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
  ( Eval (Concatenate d (Eval (AsSingleton s0)) (Eval (AsSingleton s1))) ~ s,
    HasShape s0,
    HasShape s1,
    HasShape s,
    HasShape (Eval (AsSingleton s0))
  ) =>
  SNat d ->
  Array s0 a ->
  Array s1 a ->
  Array s a
concatenate d a0 a1 = tabulate (go . fromFins)
  where
    go s =
      bool
        (index a0 (UnsafeFins s))
        ( index
            a1
            ( UnsafeFins $ insertDim
                d'
                ((s !! d') - (ds0 !! d'))
                (deleteDim d' s)
            )
        )
        ((s !! d') >= (ds0 !! d'))
    ds0 = shape (asSingleton a0)
    d' = int d

-- | Insert along a dimension at a position.
--
-- >>> pretty $ insert (SNat @2) 0 a (konst @[2,3] 0)
-- [[[0,0,1,2,3],
--   [0,4,5,6,7],
--   [0,8,9,10,11]],
--  [[0,12,13,14,15],
--   [0,16,17,18,19],
--   [0,20,21,22,23]]]
-- >>> toDynamic $ insert (SNat @0) 0 (toScalar 1) (toScalar 2)
-- UnsafeArray [2] [2,1]
insert ::
  forall s' s si d a.
  (HasShape s,
   HasShape si,
   HasShape s',
   HasShape (Eval (AsSingleton s)),
   HasShape (Eval (AsSingleton si)),
   -- FIXME si relationship
   s' ~ Eval (IncAt d (Eval (AsSingleton s)))
   ) =>
  SNat d ->
  Int ->
  Array s a ->
  Array si a ->
  Array s' a
insert sd i a b = tabulate go
  where
    go xs
      | xs' !! d == i = index (asSingleton b) (UnsafeFins (S.deleteDim d xs'))
      | xs' !! d < i = index (asSingleton a) (UnsafeFins xs')
      | otherwise = index (asSingleton a) (UnsafeFins (S.decAt d xs'))
      where xs' = fromFins xs
    d = Prelude.fromIntegral (fromSNat sd)

-- | Delete along a dimension at a position.
--
-- FIXME: What does this do???
-- >>> pretty $ delete (SNat @2) 0 a
-- [[[0,1,2],
--   [4,5,6],
--   [8,9,10]],
--  [[12,13,14],
--   [16,17,18],
--   [20,21,22]]]
delete ::
  forall d s s' a.
  (HasShape s,
   HasShape (Eval (AsSingleton s)),
   HasShape s',
   s' ~ Eval (DecAt d s)) =>
  SNat d ->
  Int ->
  Array s a ->
  Array s' a
delete sd i a = unsafeBackpermute (\s -> bool s (S.incAt d s) ((s !! d) < i)) (asSingleton a)
  where
    d = Prelude.fromIntegral (fromSNat sd)

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
  forall a d pos s si s'.
  ( HasShape (Eval (AsSingleton s)),
    HasShape (Eval (AsSingleton si)),
    s' ~ Eval (IncAt d (Eval (AsSingleton s))),
    KnownNat pos,
    pos ~ Eval (UnsafeGetIndex d s),
    HasShape s,
    HasShape si,
    HasShape s'
  ) =>
  SNat d ->
  Array s a ->
  Array si a ->
  Array s' a
append d = insert d (int (SNat :: SNat pos))

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
  forall a d pos s si s'.
  ( HasShape (Eval (AsSingleton s)),
    HasShape (Eval (AsSingleton si)),
    s' ~ Eval (IncAt d (Eval (AsSingleton s))),
    pos ~ Eval ((Fcf.-) (Eval (UnsafeGetIndex d s)) 1),
    HasShape s,
    HasShape si,
    HasShape s'
  ) =>
  SNat d ->
  Array si a ->
  Array s a ->
  Array s' a
prepend d a b = insert d 0 b a

-- | Combine two arrays as rows of a new array.
--
-- >>> pretty $ couple (array @'[3] [1,2,3]) (array @'[3] @Int [4,5,6])
-- [[1,2,3],
--  [4,5,6]]
couple :: forall a s s' se.
  (HasShape s,
   HasShape s',
   HasShape se,
   s' ~ Eval (Concatenate 0 (Eval (AsSingleton se)) (Eval (AsSingleton se))),
   se ~ Eval (InsertDim 0 1 s)
  ) =>
  Array s a -> Array s a -> Array s' a
couple a a' = concatenate (SNat @0) (elongate (SNat @0) a) (elongate (SNat @0) a')

-- | Slice along a dimension with the supplied (offset, length).
--
-- FIXME: consider putting offset and length back together in a tuple.
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
  (HasShape s,
   HasShape s',
   Eval (SetIndex d l s) ~ s') =>
  SNat d ->
  SNat off ->
  SNat l ->
  Array s a ->
  Array s' a
slice d off _ a = unsafeBackpermute (S.modifyDim (int d) (+ (int off))) a

-- | Takes the top-most elements across the supplied dimension,n tuples.
--
-- >>> pretty $ takes (Proxy :: Proxy [ '(0,1), '(1,2)]) a
-- [[[0,1,2,3],
--   [4,5,6,7]]]
takes ::
  forall ts s' s a.
  ( HasShape s,
    HasShape s',
    s' ~ Eval (ReplaceDimsT ts s)
  ) =>
  Proxy ts ->
  Array s a ->
  Array s' a
takes _ a = unsafeBackpermute id a

-- | Takes the bottom-most elements across the supplied dimension,n tuples.
--
-- >>> pretty (takeBs (Proxy :: Proxy [ '(0,1), '(1,2)]) a)
-- [[[16,17,18,19],
--   [20,21,22,23]]]
takeBs ::
  forall ts s' s a ds xs.
  ( HasShape s,
    HasShape s',
    s' ~ Eval (ReplaceDimsT ts s),
    ds ~ Eval (Map Fst ts),
    xs ~ Eval (Map Snd ts),
    HasShape ds,
    HasShape xs
  ) =>
  Proxy ts ->
  Array s a ->
  Array s' a
takeBs _ a = unsafeBackpermute (List.zipWith (+) start) a
  where
    start = List.zipWith (-) (shape a) (S.replaceDimsT (zip (shapeOf @ds) (shapeOf @xs)) (shape a))

-- | Drops the top-most elements across dimension,n tuples.
--
-- >>> pretty $ drops (Proxy :: Proxy [ '(0,1), '(2,3)]) a
-- [[[15],
--   [19],
--   [23]]]
drops ::
  forall ts s' s a.
  ( HasShape s,
    HasShape s',
    s' ~ Eval (DropDims ts s)
  ) =>
  Proxy ts ->
  Array s a ->
  Array s' a
drops _ a = unsafeBackpermute (List.zipWith (+) start) a
  where
    start = List.zipWith (-) (shapeOf @s) (shapeOf @s')

-- | Drops the bottom-most elements across dimension,n tuples.
--
-- >>> pretty $ dropBs (Proxy :: Proxy [ '(0,1), '(2,3)]) a
-- [[[0],
--   [4],
--   [8]]]
dropBs ::
  forall ts s' s a.
  ( HasShape s,
    HasShape s',
    s' ~ Eval (DropDims ts s)
  ) =>
  Proxy ts ->
  Array s a ->
  Array s' a
dropBs _ a = unsafeBackpermute id a

-- | Select by (dimension,index) pairs.
--
-- > pretty $ indexes (Proxy :: Proxy '[0,1]) [1,1] a
-- [16,17,18,19]
indexes ::
  forall ds s s' a.
  ( HasShape s,
    HasShape ds,
    HasShape s',
    s' ~ Eval (DeleteDims ds s)
  ) =>
  Proxy ds ->
  [Int] ->
  Array s a ->
  Array s' a
indexes _ xs a = unsafeBackpermute (S.insertDims (List.zip (shapeOf @ds) xs)) a

-- | Select by (dimension,index) pairs, supplying as a type.
--
-- > pretty $ indexesT (Proxy :: Proxy [ '(0,1), '(1,1)]) a
-- [16,17,18,19]
indexesT ::
  forall ds xs ts s s' a.
  ( HasShape s,
    HasShape ds,
    HasShape xs,
    HasShape s',
    s' ~ Eval (DeleteDims ds s),
    ds ~ Eval (Map Fst ts),
    xs ~ Eval (Map Snd ts)
  ) =>
  Proxy ts ->
  Array s a ->
  Array s' a
indexesT _ a = unsafeBackpermute (S.insertDims (List.zip (shapeOf @ds) (shapeOf @xs))) a

-- | Select an index /except/ along specified dimensions.
--
-- >>> let s = indexesExcept (Proxy :: Proxy '[2]) [1,1] a
-- >>> :t s
-- s :: Array '[4] Int
--
-- >>> pretty $ s
-- [16,17,18,19]
indexesExcept ::
  forall ds s s' a.
  ( HasShape s,
    HasShape ds,
    HasShape s',
    s' ~ Eval (TakeDims ds s)
  ) =>
  Proxy ds ->
  [Int] ->
  Array s a ->
  Array s' a
indexesExcept _ i a = unsafeBackpermute (\s -> insertDims (List.zip (shapeOf @ds) s) i) a

-- | Select the first element along the supplied dimensions
--
-- >>> pretty $ heads (Proxy :: Proxy '[0,2]) a
-- [0,4,8]
heads :: forall a ds s s'. (HasShape s, HasShape s', HasShape ds, s' ~ Eval (DeleteDims ds s)) => Proxy ds -> Array s a -> Array s' a
heads xs a = indexes xs (replicate (rankOf @s) zero) a

-- | Select the last element along the supplied dimensions
--
-- >>> pretty $ lasts (Proxy :: Proxy '[0,2]) a
-- [15,19,23]
lasts ::
  forall ds s s' a.
  ( HasShape s,
    HasShape ds,
    HasShape s',
    s' ~ Eval (DeleteDims ds s)
  ) =>
  Proxy ds ->
  Array s a ->
  Array s' a
lasts ds a = indexes ds lastxs a
  where
    lastxs = (\i -> shape a !! i - 1) <$> (shapeOf @ds)

-- | Select the tail elements along the supplied dimensions
--
-- >>> pretty $ tails (Proxy :: Proxy [0,2]) a
-- [[[13,14,15],
--   [17,18,19],
--   [21,22,23]]]
tails ::
  forall ds os ts s s' a ls.
  ( HasShape s,
    HasShape ds,
    HasShape s',
    HasShape ls,
    HasShape os,
    ts ~ Eval (Zip ds (Eval (Zip os ls))),
    os ~ Eval (Replicate (Eval (Rank ds)) 1),
    ls ~ Eval (Map (Flip (Fcf.-) 1) (Eval (TakeDims ds s))),
    s' ~ Eval (ReplaceDims ds ls s),
    ds ~ Eval (Map Fst ts),
    ls ~ Eval (Map Snd (Eval (Map Snd ts))),
    os ~ Eval (Map Fst (Eval (Map Snd ts)))
  ) =>
  Proxy ds ->
  Array s a ->
  Array s' a
tails _ a = slices (Proxy :: Proxy ts) a

-- | Select the init elements along the supplied dimensions
--
-- >>> pretty $ inits (Proxy :: Proxy [0,2]) a
-- [[[0,1,2],
--   [4,5,6],
--   [8,9,10]]]
inits ::
  forall ds os ts s s' a ls.
  ( HasShape s,
    HasShape ds,
    HasShape s',
    HasShape ls,
    HasShape os,
    ts ~ Eval (Zip ds (Eval (Zip os ls))),
    os ~ Eval (Replicate (Eval (Rank ds)) 0),
    ls ~ Eval (Map (Flip (Fcf.-) 1) (Eval (TakeDims ds s))),
    s' ~ Eval (ReplaceDims ds ls s),
    ds ~ Eval (Map Fst ts),
    ls ~ Eval (Map Snd (Eval (Map Snd ts))),
    os ~ Eval (Map Fst (Eval (Map Snd ts)))
  ) =>
  Proxy ds ->
  Array s a ->
  Array s' a
inits _ a = slices (Proxy :: Proxy ts) a

-- | Slice along a dimension with the supplied (offset, length).
--
-- >>> pretty $ slices (Proxy :: Proxy '[ '(2, '(1,2))]) a
-- [[[1,2],
--   [5,6],
--   [9,10]],
--  [[13,14],
--   [17,18],
--   [21,22]]]
slices ::
  forall a ds ls os s s' ts.
  (HasShape s,
   HasShape s',
   HasShape ds,
   HasShape ls,
   HasShape os,
   ds ~ Eval (Map Fst ts),
   ls ~ Eval (Map Snd (Eval (Map Snd ts))),
   os ~ Eval (Map Fst (Eval (Map Snd ts))),
   Eval (ReplaceDims ds ls s) ~ s') =>
  Proxy ts ->
  Array s a ->
  Array s' a
slices _ a = unsafeBackpermute (List.zipWith (+) o) a
  where
    o = S.replaceDims (shapeOf @ds) (shapeOf @os) (replicate (rank a) 0)

-- | Extracts dimensions to an outer layer.
--
-- > a == (fromScalar <$> extracts [0..rank a] a)
--
-- >>> pretty $ shape <$> extracts (Proxy :: Proxy '[0]) a
-- [[3,4],[3,4]]
extracts ::
  forall ds st si so a.
  ( HasShape st,
    HasShape ds,
    HasShape si,
    HasShape so,
    si ~ Eval (DeleteDims ds st),
    so ~ Eval (TakeDims ds st)
  ) =>
  Proxy ds ->
  Array st a ->
  Array so (Array si a)
extracts d a = tabulate (\s -> indexes d (fromFins s) a)

-- | Extracts /except/ dimensions to an outer layer.
--
-- >>> let e = extractsExcept (Proxy :: Proxy '[1,2]) a
-- >>> pretty $ shape <$> e
-- [[3,4],[3,4]]
extractsExcept ::
  forall ds st si so a.
  ( HasShape st,
    HasShape ds,
    HasShape si,
    HasShape so,
    so ~ Eval (DeleteDims ds st),
    si ~ Eval (TakeDims ds st)
  ) =>
  Proxy ds ->
  Array st a ->
  Array so (Array si a)
extractsExcept d a = tabulate go
  where
    go s = indexesExcept d (fromFins s) a

-- | Reduce along specified dimensions, using the supplied fold.
--
-- >>> pretty $ reduces (Proxy :: Proxy '[0]) sum a
-- [66,210]
-- >>> pretty $ reduces (Proxy :: Proxy [0,2]) sum a
-- [[12,15,18,21],
--  [48,51,54,57]]
--
reduces ::
  forall ds st si so a b.
  ( HasShape st,
    HasShape ds,
    HasShape si,
    HasShape so,
    si ~ Eval (DeleteDims ds st),
    so ~ Eval (TakeDims ds st)
  ) =>
  Proxy ds ->
  (Array si a -> b) ->
  Array st a ->
  Array so b
reduces ds f a = fmap f (extracts ds a)

-- | Join inner and outer dimension layers by supplied dimensions. No checks on shape.
--
-- >>> let e = extracts (Proxy :: Proxy [1,0]) a
-- >>> let j = joins (Proxy :: Proxy [1,0]) e
-- >>> a == j
-- True
joins ::
  forall a ds si so st.
  (HasShape ds,
   HasShape st,
   HasShape si,
   HasShape so,
   Eval (InsertDims (Eval (Zip ds so)) si) ~ st) =>
  Proxy ds ->
  Array so (Array si a) ->
  Array st a
joins _ a = tabulate go
  where
    go s = index (index a (UnsafeFins $ S.takeDims (shapeOf @ds) (fromFins s))) (UnsafeFins $ S.deleteDims (shapeOf @ds) (fromFins s))

-- | Join inner and outer dimension layers in outer dimension order.
--
-- >>> a == join (extracts (Proxy :: Proxy [0,1]) a)
-- True
join ::
  forall a si so st.
  (HasShape st,
   HasShape si,
   HasShape so,
   Eval ((++) so si) ~ st) =>
  Array so (Array si a) ->
  Array st a
join a = tabulate go
  where
    go s = index (index a (UnsafeFins $ S.takeDims ds (fromFins s))) (UnsafeFins $ S.deleteDims ds (fromFins s))
    ds = [0..rankOf @so - 1]

-- | Traverse along specified dimensions.
--
-- FIXME: Need proofs.
traverses ::
  (Applicative f,
   HasShape s,
   HasShape s',
   s' ~ Eval (InsertDims (Eval (Zip ds (Eval (TakeDims ds s)))) (Eval (DeleteDims ds s))),
   HasShape (Eval (InsertDims (Eval (Zip ds (Eval (TakeDims ds s)))) (Eval (DeleteDims ds s)))),
   HasShape (Eval (DeleteDims ds s)),
   HasShape (Eval (TakeDims ds s)),
   HasShape ds) =>
  Proxy ds ->
  (a -> f b) ->
  Array s a ->
  f (Array s' b)
traverses ds f a = joins ds <$> traverse (traverse f) (extracts ds a)

-- | Maps a function along specified dimensions.
--
-- > :t maps (transpose) (Proxy :: Proxy '[1]) a
-- maps (transpose) (Proxy :: Proxy '[1]) a :: Array [4, 3, 2] Int
maps ::
  forall ds st st' si si' so a b.
  ( HasShape st,
    HasShape st',
    HasShape ds,
    HasShape si,
    HasShape si',
    HasShape so,
    si ~ Eval (DeleteDims ds st),
    so ~ Eval (TakeDims ds st),
    st' ~ Eval (InsertDims (Eval (Zip ds so)) si'),
    st ~ Eval (InsertDims (Eval (Zip ds so)) si)
  ) =>
  (Array si a -> Array si' b) ->
  Proxy ds ->
  Array st a ->
  Array st' b
maps f d a = joins d (fmapRep f (extracts d a))

-- | Filters along specified dimensions (which are flattened as a dynamic array).
--
-- >>> pretty $ filters (Proxy :: Proxy [0,1]) (any ((==0) . (`mod` 7))) a
-- [[0,1,2,3],[4,5,6,7],[12,13,14,15],[20,21,22,23]]
filters ::
  forall ds si so a.
  ( HasShape ds,
    HasShape si,
    HasShape so,
    si ~ Eval (DeleteDims ds so),
    HasShape (Eval (TakeDims ds so))
  ) =>
  Proxy ds ->
  (Array si a -> Bool) ->
  Array so a ->
  D.Array (Array si a)
filters ds p a = D.asArray $ V.filter p $ asVector (extracts ds a)

-- | Zips two arrays with a function along specified dimensions.
--
-- >>> pretty $ zips (Proxy :: Proxy [0,1]) (zipWith (,)) a (reverses [0] a)
-- [[[(0,12),(1,13),(2,14),(3,15)],
--   [(4,16),(5,17),(6,18),(7,19)],
--   [(8,20),(9,21),(10,22),(11,23)]],
--  [[(12,0),(13,1),(14,2),(15,3)],
--   [(16,4),(17,5),(18,6),(19,7)],
--   [(20,8),(21,9),(22,10),(23,11)]]]
zips ::
  forall ds st st' si si' so a b c.
  ( HasShape st,
    HasShape st',
    HasShape ds,
    HasShape si,
    HasShape si',
    HasShape so,
    si ~ Eval (DeleteDims ds st),
    so ~ Eval (TakeDims ds st),
    st' ~ Eval (InsertDims (Eval (Zip ds so)) si'),
    st ~ Eval (InsertDims (Eval (Zip ds so)) si)
  ) =>
  Proxy ds ->
  (Array si a -> Array si b -> Array si' c) ->
  Array st a ->
  Array st b ->
  Array st' c
zips ds f a b = joins ds (zipWith f (extracts ds a) (extracts ds b))

-- | Modify using the supplied function along (dimension, position) tuples.
--
-- >>> pretty $ modifies (fmap (100+)) (Proxy :: Proxy '[ '(2,0)]) a
-- [[[100,1,2,3],
--   [104,5,6,7],
--   [108,9,10,11]],
--  [[112,13,14,15],
--   [116,17,18,19],
--   [120,21,22,23]]]
modifies ::
  forall a si s ts ds ps so.
  ( HasShape s,
    HasShape si,
    HasShape so,
    HasShape ds,
    HasShape ps,
    ds ~ Eval (Map Fst ts),
    ps ~ Eval (Map Snd ts),
    si ~ Eval (DeleteDims ds s),
    so ~ Eval (TakeDims ds s),
    s ~ Eval (InsertDims (Eval (Zip ds so)) si)) =>
  (Array si a -> Array si a) ->
  Proxy ts ->
  Array s a ->
  Array s a
modifies f _ a = joins (Proxy :: Proxy ds) $ modify (UnsafeFins $ shapeOf @ps) f (extracts (Proxy :: Proxy ds) a)

-- | Apply a binary function between successive slices, across (dimension, lag) tuples
--
-- >>> pretty $ diffs (Proxy :: Proxy '[ '(1,1)]) (zipWith (-)) a
-- [[[4,4,4,4],
--   [4,4,4,4]],
--  [[4,4,4,4],
--   [4,4,4,4]]]
diffs ::
  forall a b ts ds ls si si' st st' so postDrop.
  ( HasShape ds,
    HasShape ls,
    HasShape si,
    HasShape si',
    HasShape st,
    HasShape st',
    HasShape so,
    HasShape postDrop,
    si ~ Eval (DeleteDims ds postDrop),
    so ~ Eval (TakeDims ds postDrop),
    st' ~ Eval (InsertDims (Eval (Zip ds so)) si'),
    postDrop ~ Eval (InsertDims (Eval (Zip ds so)) si),
    ds ~ Eval (Map Fst ts),
    ls ~ Eval (Map Snd ts),
    postDrop ~ Eval (DropDims ts st)
  ) =>
  Proxy ts ->
  (Array si a -> Array si a -> Array si' b) -> Array st a -> Array st' b
diffs ts f a = zips (Proxy :: Proxy ds) f (drops ts a) (dropBs ts a)

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
  ( HasShape sa,
    HasShape sb,
    HasShape sc,
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
  ( HasShape sa,
    HasShape sb,
    HasShape sc,
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
-- > let b = array [1..6] :: Array [2,3] Int
-- > pretty $ contract sum [1,2] (expand (*) b (transpose b))
-- [[14,32],
--  [32,77]]
contract ::
  forall a b s ss s' ds.
  ( KnownNat (Eval (Minimum (Eval (TakeDims ds s)))),
    HasShape (Eval (TakeDims ds s)),
    HasShape s,
    HasShape ds,
    HasShape ss,
    HasShape s',
    s' ~ Eval (DeleteDims ds s),
    ss ~ '[Eval (Minimum (Eval (TakeDims ds s)))]
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
-- > let b = array [1..6] :: Array [2,3] Int
-- > pretty $ dot sum (*) b (transpose b)
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
-- > pretty $ dot sum (*) v b
-- [9,12,15]
--
-- > pretty $ dot sum (*) b v
-- [14,32]
dot ::
  forall a b c d sa sb s' ss se.
  ( HasShape sa,
    HasShape sb,
    HasShape (Eval ((++) sa sb)),
    se ~ Eval (TakeDims '[Eval (Rank sa) - 1, Eval (Rank sa)] (Eval ((++) sa sb))),
    HasShape se,
    KnownNat (Eval (Minimum se)),
    KnownNat (Eval (Rank sa) - 1),
    KnownNat (Eval (Rank sa)),
    ss ~ '[Eval (Minimum se)],
    HasShape ss,
    s' ~ Eval (DeleteDims '[Eval (Rank sa) - 1, Eval (Rank sa)] (Eval ((++) sa sb))),
    HasShape s'
  ) =>
  (Array ss c -> d) ->
  (a -> b -> c) ->
  Array sa a ->
  Array sb b ->
  Array s' d
dot f g a b = contract f (Proxy :: Proxy '[Eval (Rank sa) - 1, Eval (Rank sa)]) (expand g a b)

-- | Array multiplication.
--
-- matrix multiplication
--
-- > let b = array [1..6] :: Array [2,3] Int
-- > pretty $ mult b (transpose b)
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
-- > pretty $ mult v b
-- [9,12,15]
--
-- > pretty $ mult b v
-- [14,32]
mult ::
  forall a sa sb s' ss se.
  ( Additive a,
    Multiplicative a,
    HasShape sa,
    HasShape sb,
    HasShape (Eval ((++) sa sb)),
    se ~ Eval (TakeDims '[Eval (Rank sa) - 1, Eval (Rank sa)] (Eval ((++) sa sb))),
    HasShape se,
    KnownNat (Eval (Minimum se)),
    KnownNat (Eval (Rank sa) - 1),
    KnownNat (Eval (Rank sa)),
    ss ~ '[Eval (Minimum se)],
    HasShape ss,
    s' ~ Eval (DeleteDims '[Eval (Rank sa) - 1, Eval (Rank sa)] (Eval ((++) sa sb))),
    HasShape s'
  ) =>
  Array sa a ->
  Array sb a ->
  Array s' a
mult = dot sum (*)

-- | windows xs are xs-sized windows of an array
--
-- >>> shape $ windows (Proxy :: Proxy [2,2]) (range @[4,3,2])
-- [3,2,2,2,2]
windows :: forall w s ws a.
  ( HasShape s,
    HasShape w,
    HasShape ws,
    ws ~ Eval (ExpandWindows w s)) =>
  Proxy w -> Array s a -> Array ws a
windows _ a = unsafeBackpermute (S.indexWindows (rankOf @w)) a

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
   HasShape si,
   HasShape s,
   HasShape s',
   HasShape re,
   HasShape i',
   KnownNat r,
   HasShape ws,
   ws ~ Eval (ExpandWindows i' s),
   r ~ Eval (Rank s),
   i' ~ Eval (Rerank r si),
   re ~ Eval ((Fcf.++) (Eval (Range (Eval (Rank s)))) (Eval (EnumFromTo (r + r) (Eval ((Fcf.-) (Eval (Rank ws)) 1))))),
   i' ~ Eval (DeleteDims re ws),
   s' ~ Eval (TakeDims re ws)
  ) =>
  Array si a -> Array s a -> Array s' Bool
find i a = xs
  where
    i' = rerank (SNat @r) i
    ws = windows (Proxy :: Proxy i') a
    xs = fmap (== i') (extracts (Proxy :: Proxy re) ws)

-- | Check if the first array is a prefix of the second
--
-- FIXME: different to D.isPrefixOf result
-- >>> isPrefixOf (array @[2,2] [0,4,12,16]) a
-- True
isPrefixOf ::
  forall s' s a.
  (Eq a,
   HasShape s,
   HasShape s',
   True ~ Eval (ShapeLTE s' (Eval (Rerank (Eval (Rank s')) s)))) =>
  Array s' a -> Array s a -> Bool
isPrefixOf p a = p == cut a

-- | Check if the first array is a suffix of the second
--
-- >>> isSuffixOf (array @[2,2] [18,19,22,23]) a
-- True
isSuffixOf ::
  forall s' s r a.
  (Eq a,
   HasShape s,
   HasShape s',
   KnownNat r,
   HasShape (Eval (Rerank r s)),
   r ~ Eval (Rank s'),
   True ~ Eval (ShapeLTE s' (Eval (Rerank (Eval (Rank s')) s)))) =>
  Array s' a -> Array s a -> Bool
isSuffixOf p a = p == cutSuffix a

-- | Check if the first array is an infix of the second
--
-- >>> isInfixOf (array @[2,2] [18,19,22,23]) a
-- True
isInfixOf ::
  forall s' si s a r i' re ws.
  (Eq a,
   HasShape si,
   HasShape s,
   HasShape s',
   HasShape re,
   HasShape i',
   KnownNat r,
   HasShape ws,
   ws ~ Eval (ExpandWindows i' s),
   r ~ Eval (Rank s),
   i' ~ Eval (Rerank r si),
   re ~ Eval ((Fcf.++) (Eval (Range (Eval (Rank s)))) (Eval (EnumFromTo (r + r) (Eval ((Fcf.-) (Eval (Rank ws)) 1))))),
   i' ~ Eval (DeleteDims re ws),
   s' ~ Eval (TakeDims re ws)
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
  (HasShape s,
   HasShape s') =>
  a -> Array s a -> Array s' a
fill x (Array v) = Array (V.take (S.size (shapeOf @s')) (v <> V.replicate (S.size (shapeOf @s') - V.length v) x))

-- | Cut an array to form a new (smaller) shape. Errors if the new shape is larger. The old array is reranked to the rank of the new shape first.
--
-- >>> toDynamic $ cut @'[2] (array @'[4] @Int [0..3])
-- UnsafeArray [2] [0,1]
cut ::
  forall s' s a.
  (HasShape s,
   HasShape s',
   True ~ Eval (ShapeLTE s' (Eval (Rerank (Eval (Rank s')) s)))) =>
  Array s a ->
  Array s' a
cut a = unsafeBackpermute id a

-- | Cut an array to form a new (smaller) shape, using suffix elements. Errors if the new shape is larger. The old array is reranked to the rank of the new shape first.
--
-- >>> toDynamic $ cutSuffix @[2,2] a
-- UnsafeArray [2,2] [18,19,22,23]
cutSuffix ::
  forall s' s a r.
  (HasShape s,
   HasShape s',
   KnownNat r,
   HasShape (Eval (Rerank r s)),
   r ~ Eval (Rank s'),
   True ~ Eval (ShapeLTE s' (Eval (Rerank (Eval (Rank s')) s)))) =>
  Array s a ->
  Array s' a
cutSuffix a = unsafeBackpermute (List.zipWith (+) diffDim) a'
  where
    a' = rerank (SNat @r) a
    diffDim = List.zipWith (-) (shape a') (shapeOf @s')

-- | Pad an array to form a new shape, supplying a default value for elements outside the shape of the old array. The old array is reranked to the rank of the new shape first.
--
-- >>> toDynamic $ pad @'[5] 0 (array @'[4] @Int [0..3])
-- UnsafeArray [5] [0,1,2,3,0]
pad ::
  forall s' a s r.
  (HasShape s,
   HasShape s',
   KnownNat r,
   HasShape (Eval (Rerank r s)),
   r ~ Eval (Rank s')) =>
  a ->
  Array s a ->
  Array s' a
pad d a = tabulate (\s -> bool d (index a' (unsafeCoerce s)) ((fromFins s) `S.inside` (shape a')))
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
  (HasShape s,
   HasShape s',
   KnownNat r,
   HasShape (Eval (Rerank r s)),
   r ~ Eval (Rank s')) =>
  a ->
  Array s a ->
  Array s' a
lpad d a = tabulate (\s -> bool d (index a' (UnsafeFins $ olds s)) ((olds s) `S.inside` (shape a')))
  where
    a' = rerank (SNat @r) a
    gap = List.zipWith (-) (shapeOf @s') (shape a')
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
    HasShape s,
    HasShape s'
  ) =>
  Array s a ->
  Array s' a
reshape = unsafeBackpermute (shapen s . flatten s')
  where
    s = shapeOf @s
    s' = shapeOf @s'

-- | Make an Array single dimensional
--
-- >>> pretty $ flat (range @[2,2])
-- [0,1,2,3]
-- >>> pretty (flat $ toScalar 0)
-- [0]
flat :: forall s' s a. (HasShape s, HasShape s', s' ~ ('[Eval (Size s)])) => Array s a -> Array s' a
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
  (HasShape s,
   HasShape s',
   Eval (IsPrefixOf s s') ~ True) =>
  Array s a ->
  Array s' a
repeat a = unsafeBackpermute (List.drop (S.rank (shapeOf @s') - rank a)) a

-- | Reshape an array, cycling through the elements without regard to the original shape.
--
-- >>> pretty $ cycle @[2,2,2] (array @'[3] [1,2,3])
-- [[[1,2],
--   [3,1]],
--  [[2,3],
--   [1,2]]]
cycle ::
  forall s' s a.
  (HasShape s,
   HasShape s') =>
  Array s a ->
  Array s' a
cycle a = unsafeBackpermute (S.shapen (shape a) . (`mod` (size a)) . S.flatten (shapeOf @s')) a

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
  (HasShape s,
   HasShape s',
   s' ~ Eval (Rerank r s)) =>
  SNat r -> Array s a -> Array s' a
rerank _ a = unsafeModifyShape a

-- | Change the order of dimensions.
--
-- >>> pretty $ reorder (Proxy :: Proxy [2,0,1]) a
-- [[[0,4,8],
--   [12,16,20]],
--  [[1,5,9],
--   [13,17,21]],
--  [[2,6,10],
--   [14,18,22]],
--  [[3,7,11],
--   [15,19,23]]]
reorder ::
  forall dims s s' a.
  (HasShape s,
   HasShape s',
   HasShape dims,
   s' ~ Eval (Reorder s dims)
  ) =>
  Proxy dims ->
  Array s a ->
  Array s' a
reorder _ a = unsafeBackpermute (\s -> S.insertDims (List.zip (shapeOf @dims) s) []) a

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
  (HasShape s,
   HasShape t,
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
  (HasShape s,
   HasShape s',
   s' ~ Eval (InsertDim d 1 s)) =>
  SNat d ->
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
  forall a s s'. (HasShape s, HasShape s', s' ~ Eval (Reverse s)) => Array s a -> Array s' a
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
  (HasShape s,
   HasShape s',
   Eval (IsFin d (Eval (Rank s))) ~ True,
   s' ~ Eval (InsertDim d x s)) =>
  SNat d ->
  SNat x ->
  Array s a ->
  Array s' a
inflate d _ a = unsafeBackpermute (S.deleteDim (int d)) a

-- | Concatenate and replace dimensions, creating a new dimension at the supplied postion.
--
-- >>> pretty $ concats (Proxy :: Proxy [0,1]) (SNat @1) a
-- [[0,4,8,12,16,20],
--  [1,5,9,13,17,21],
--  [2,6,10,14,18,22],
--  [3,7,11,15,19,23]]
concats ::
  forall s s' newd ds a.
  (HasShape s,
   HasShape s',
   HasShape ds,
   s' ~ Eval (InsertDim newd (Eval (Size (Eval (TakeDims ds s)))) (Eval (DeleteDims ds s)))) =>
  Proxy ds ->
  SNat newd ->
  Array s a ->
  Array s' a
concats _ newd a = unsafeBackpermute unconcatDims a
  where
    unconcatDims s = S.insertDims (List.zip ds (S.shapen (S.takeDims ds (shape a)) (S.unsafeGetIndex n s))) (S.deleteDim n s)
    n = int newd
    ds = shapeOf @ds

-- | Reverses element order along specified dimensions.
--
-- >>> pretty $ reverses [0,1] a
-- [[[20,21,22,23],
--   [16,17,18,19],
--   [12,13,14,15]],
--  [[8,9,10,11],
--   [4,5,6,7],
--   [0,1,2,3]]]
reverses ::
  (HasShape s) =>
  [Int] ->
  Array s a ->
  Array s a
reverses ds a = unsafeBackpermute (S.reverseIndex ds (shape a)) a

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
  (HasShape s) =>
  Int ->
  Int ->
  Array s a ->
  Array s a
rotate d r a = unsafeBackpermute (S.modifyDim d (\i -> (r + i) `mod` (shape a !! d))) a

-- | Rotate an array by/along offset,dimension tuples.
--
-- >>> pretty $ rotates [(1, 2)] a
-- [[[8,9,10,11],
--   [0,1,2,3],
--   [4,5,6,7]],
--  [[20,21,22,23],
--   [12,13,14,15],
--   [16,17,18,19]]]
rotates ::
  forall a s.
  (HasShape s) =>
  [(Int, Int)] ->
  Array s a ->
  Array s a
rotates rs a = unsafeBackpermute (rotateIndex rs (shapeOf @s)) a

-- | Sort an array along the supplied dimensions.
--
-- >>> pretty $ sorts (Proxy :: Proxy '[0]) (array @[2,2] [2,3,1,4])
-- [[1,4],
--  [2,3]]
-- >>> pretty $ sorts (Proxy :: Proxy '[1]) (array @[2,2] [2,3,1,4])
-- [[2,3],
--  [1,4]]
-- >>> pretty $ sorts (Proxy :: Proxy '[0,1]) (array @[2,2] [2,3,1,4])
-- [[1,2],
--  [3,4]]
sorts ::
  forall ds s a si so.
  (Ord a,
   HasShape s,
   HasShape ds,
   HasShape si,
   HasShape so,
   si ~ Eval (DeleteDims ds s),
   so ~ Eval (TakeDims ds s),
   s ~ Eval (InsertDims (Eval (Zip ds so)) si)
  ) =>
  Proxy ds -> Array s a -> Array s a
sorts ds a = joins ds $ unsafeModifyVector sortV (extracts ds a)

-- | The indices into the array if it were sorted by a comparison function along the dimensions supplied.
--
-- >>> import Data.Ord (Down (..))
-- >>> toDynamic $ sortsBy (Proxy :: Proxy '[0]) (fmap Down) (array @[2,2] [2,3,1,4])
-- UnsafeArray [2,2] [2,3,1,4]
sortsBy ::
  forall ds s a b si so.
  (Ord b,
   HasShape s,
   HasShape ds,
   HasShape si,
   HasShape so,
   si ~ Eval (DeleteDims ds s),
   so ~ Eval (TakeDims ds s),
   s ~ Eval (InsertDims (Eval (Zip ds so)) si)
  ) =>
  Proxy ds -> (Array si a -> Array si b) -> Array s a -> Array s a
sortsBy ds c a = joins ds $ unsafeModifyVector (sortByV c) (extracts ds a)

-- | The indices into the array if it were sorted along the dimensions supplied.
--
-- >>> orders (Proxy :: Proxy '[0]) (array @[2,2] [2,3,1,4])
-- [1,0]
orders ::
  forall ds s a si so.
  (Ord a,
   HasShape s,
   HasShape ds,
   HasShape si,
   HasShape so,
   si ~ Eval (DeleteDims ds s),
   so ~ Eval (TakeDims ds s),
   s ~ Eval (InsertDims (Eval (Zip ds so)) si)
  ) =>
  Proxy ds -> Array s a -> Array so Int
orders ds a = unsafeModifyVector orderV (extracts ds a)

-- | The indices into the array if it were sorted by a comparison function along the dimensions supplied.
--
-- >>> import Data.Ord (Down (..))
-- >>> ordersBy (Proxy :: Proxy '[0]) (fmap Down) (array @[2,2] [2,3,1,4])
-- [0,1]
ordersBy ::
  forall ds s a b si so.
  (Ord b,
   HasShape s,
   HasShape ds,
   HasShape si,
   HasShape so,
   si ~ Eval (DeleteDims ds s),
   so ~ Eval (TakeDims ds s),
   s ~ Eval (InsertDims (Eval (Zip ds so)) si)
  ) =>
  Proxy ds -> (Array si a -> Array si b) -> Array s a -> Array so Int
ordersBy ds c a = unsafeModifyVector (orderByV c) (extracts ds a)

-- | Apply a binary array function to two arrays with matching shapes across the supplied (matching) dimensions.
--
-- >>> a = array @[2,3] [0..5]
-- >>> b = array @'[3] [6..8]
-- >>> pretty $ telecasts (Proxy :: Proxy '[1]) (Proxy :: Proxy '[0]) (concatenate (SNat @0)) a b
-- [[0,3,6],
--  [1,4,7],
--  [2,5,8]]
telecasts ::
  forall sa sb sc sia sib sic soa sob soc ma mb a b c.
  (HasShape sa,
   HasShape sb,
   HasShape sc,
   HasShape sia,
   HasShape sib,
   HasShape sic,
   HasShape soa,
   HasShape sob,
   HasShape soc,
   HasShape ma,
   HasShape mb,
   soa ~ Eval (TakeDims ma sa),
   sob ~ Eval (TakeDims mb sb),
   sia ~ Eval (DeleteDims ma sa),
   sib ~ Eval (DeleteDims mb sb),
   soa ~ sob,
   soc ~ Eval ((++) soa sic),
   sc ~ soc) =>
  Proxy ma -> Proxy mb -> (Array sia a -> Array sib b -> Array sic c) -> Array sa a -> Array sb b -> Array sc c
telecasts ma mb f a b = join (zipWith f (extracts ma a) (extracts mb b))

-- | Apply a binary array function to two arrays where the shape of the first array is a prefix of the second array.
--
-- >>> a = array @[2,3] [0..5]
-- >>> pretty $ transmit (zipWith (+)) (toScalar 1) a
-- [[1,2,3],
--  [4,5,6]]
--
transmit ::
  forall sa sb sc a b c ds sib sic sob.
  (HasShape sa,
   HasShape sb,
   HasShape sc,
   HasShape ds,
   HasShape sib,
   HasShape sic,
   HasShape sob,
   ds ~ Eval (EnumFromTo (Eval (Rank sa)) (Eval ((Fcf.-) (Eval (Rank sb)) 1))),
   sib ~ Eval (DeleteDims ds sb),
   sob ~ Eval (TakeDims ds sb),
   sb ~ Eval (InsertDims (Eval (Zip ds sob)) sib),
   sc ~ Eval (InsertDims (Eval (Zip ds sob)) sic),
   True ~ (Eval (IsPrefixOf sa sb))) =>
  (Array sa a -> Array sib b -> Array sic c) -> Array sa a -> Array sb b -> Array sc c
transmit f a b = maps (f a) (Proxy :: Proxy ds) b

-- | <https://en.wikipedia.org/wiki/Vector_(mathematics_and_physics) Wiki Vector>
type Vector s a = Array '[s] a

data SomeVector a where
  SomeVector :: KnownNat n => Vector n a -> SomeVector a

deriving instance (Show a) => Show (SomeVector a)

withLength :: forall n a r. Vector n a -> (KnownNat n => r) -> r
withLength v r = case someNatVal (fromIntegral $ V.length (asVector v)) of
  SomeNat (Proxy :: Proxy n') -> case unsafeCoerce Refl of
    (Refl :: n :~: n') -> r

aVector :: FromVector t a => t -> SomeVector a
aVector (Array . asVector -> v) = withLength v (SomeVector v)

example_append :: (Show a, Num a, FromInteger a) => SomeVector a -> String
example_append (SomeVector a) = show (append (SNat @0) a (toScalar 0))

example_insert :: (Show a, FromInteger a) => SomeVector a -> String
example_insert (SomeVector a) = show (insert (SNat @0) 0 a (toScalar 0))

data SomeVector' a = forall n. SomeVector' (SNat n) (Vector n a)

deriving instance (Show a) => Show (SomeVector' a)

someVector' :: FromVector t a => KnownNat n => SNat n -> t -> SomeVector' a
someVector' n t = SomeVector' n (vector' n t)

aVector' :: forall a t. FromVector t a => t -> SomeVector' a
aVector' t = withSomeSNat (fromIntegral $ V.length (asVector t)) $ \(SNat :: SNat n) -> SomeVector' SNat (vector' (SNat @n) (asVector t))

example_insert' :: (Show a, FromInteger a) => SomeVector' a -> String
example_insert' (SomeVector' (SNat :: SNat n) a) = show (insert (SNat @0) 0 a (toScalar 0))

example_append' :: (Show a, Num a, FromInteger a) => SomeVector' a -> String
example_append' (SomeVector' (SNat :: SNat n) a) = show (append (SNat @0) a (toScalar 0))

instance (Arbitrary a) => Arbitrary (SomeVector' a) where
  arbitrary = do
    n <- arbitrary
    v <- V.replicateM (Prelude.fromIntegral n) arbitrary
    withSomeNat n $ \sn -> pure (someVector' sn v)

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
    KnownNat m,
    HasShape '[m, m]
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
    KnownNat m,
    HasShape '[m, m]
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
  forall st s sh a pos.
  ( HasShape st,
    HasShape s,
    HasShape sh,
    KnownNat pos,
    HasShape (Eval (AsSingleton st)),
    HasShape (Eval (AsSingleton sh)),
    s ~ Eval (IncAt 0 (Eval (AsSingleton st))),
    pos ~ Eval ((Fcf.-) (Eval (UnsafeGetIndex 0 st)) 1)) =>
    Array sh a -> Array st a -> Array s a
cons =
  prepend (SNat @0)

-- | Add a new row at the end
--
-- >>> pretty $ snoc (array @[2,2] [0,1,2,3]) (array @'[2] [4,5])
-- [[0,1],
--  [2,3],
--  [4,5]]
snoc :: forall si s sl a pos.
  ( HasShape si,
    HasShape s,
    HasShape sl,
    HasShape (Eval (AsSingleton si)),
    HasShape (Eval (AsSingleton sl)),
    s ~ Eval (IncAt 0 (Eval (AsSingleton si))),
    KnownNat pos,
    pos ~ Eval (UnsafeGetIndex 0 si)) =>
    Array si a -> Array sl a -> Array s a
snoc = append (SNat @0)

-- | split an array into the first row and the remaining rows.
--
-- >>> import Data.Bifunctor (bimap)
-- >>> bimap toDynamic toDynamic $ uncons (array @[3,2] [0..5])
-- (UnsafeArray [2] [0,1],UnsafeArray [2,2] [2,3,4,5])
uncons ::
  forall a s sh st ls os ts ds.
  (HasShape s,
   HasShape sh,
   HasShape st,
   ds ~ '[0],
   HasShape (Eval (AsSingleton s)),
   sh ~ Eval (DeleteDims ds (Eval (AsSingleton s))),
   HasShape ls,
   HasShape os,
   ts ~ Eval (Zip ds (Eval (Zip os ls))),
   os ~ Eval (Replicate (Eval (Rank ds)) 1),
   ls ~ Eval (Map (Flip (Fcf.-) 1) (Eval (TakeDims ds (Eval (AsSingleton s))))),
   st ~ Eval (ReplaceDims ds ls (Eval (AsSingleton s))),
   ds ~ Eval (Map Fst ts),
   ls ~ Eval (Map Snd (Eval (Map Snd ts))),
   os ~ Eval (Map Fst (Eval (Map Snd ts)))
  ) =>
  Array s a -> (Array sh a, Array st a)
uncons a = (heads (Proxy :: Proxy '[0]) a', tails (Proxy :: Proxy '[0]) a')
  where
    a' = asSingleton a

-- | split an array into the initial rows and the last row.
--
-- >>> import Data.Bifunctor (bimap)
-- >>> bimap toDynamic toDynamic $ unsnoc (array @[3,2] [0..5])
-- (UnsafeArray [2,2] [0,1,2,3],UnsafeArray [2] [4,5])
unsnoc ::
  forall ds os ts s a ls si sl.
  ( HasShape s,
    HasShape ds,
    HasShape si,
    HasShape ls,
    HasShape os,
    HasShape sl,
    HasShape (Eval (AsSingleton s)),
    ds ~ '[0],
    ts ~ Eval (Zip ds (Eval (Zip os ls))),
    os ~ Eval (Replicate (Eval (Rank ds)) 0),
    ls ~ Eval (Map (Flip (Fcf.-) 1) (Eval (TakeDims ds (Eval (AsSingleton s))))),
    si ~ Eval (ReplaceDims ds ls (Eval (AsSingleton s))),
    ds ~ Eval (Map Fst ts),
    ls ~ Eval (Map Snd (Eval (Map Snd ts))),
    os ~ Eval (Map Fst (Eval (Map Snd ts))),
    sl ~ Eval (DeleteDims ds (Eval (AsSingleton s)))
  ) => Array s a -> (Array si a, Array sl a)
unsnoc a = (inits (Proxy :: Proxy '[0]) a', lasts (Proxy :: Proxy '[0]) a')
  where
    a' = asSingleton a

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
  forall s sh st a pos ts os ls ds.
  (HasShape s,
   HasShape sh,
   HasShape st,
   KnownNat pos,
   HasShape (Eval (AsSingleton st)),
   HasShape (Eval (AsSingleton sh)),
   s ~ Eval (IncAt 0 (Eval (AsSingleton st))),
   pos ~ Eval ((Fcf.-) (Eval (UnsafeGetIndex 0 st)) 1),
   ds ~ '[0],
   HasShape (Eval (AsSingleton s)),
   sh ~ Eval (DeleteDims ds (Eval (AsSingleton s))),
   HasShape ls,
   HasShape os,
   ts ~ Eval (Zip ds (Eval (Zip os ls))),
   os ~ Eval (Replicate (Eval (Rank ds)) 1),
   ls ~ Eval (Map (Flip (Fcf.-) 1) (Eval (TakeDims ds (Eval (AsSingleton s))))),
   st ~ Eval (ReplaceDims ds ls (Eval (AsSingleton s))),
   ds ~ Eval (Map Fst ts),
   ls ~ Eval (Map Snd (Eval (Map Snd ts))),
   os ~ Eval (Map Fst (Eval (Map Snd ts)))) =>
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
  forall si sl s a pos ds ls os ts.
  (HasShape si,
   HasShape sl,
   HasShape s,
   HasShape (Eval (AsSingleton si)),
   HasShape (Eval (AsSingleton sl)),
   s ~ Eval (IncAt 0 (Eval (AsSingleton si))),
   KnownNat pos,
   pos ~ Eval (UnsafeGetIndex 0 si),
   HasShape ds,
   HasShape ls,
   HasShape os,
   HasShape (Eval (AsSingleton s)),
   ds ~ '[0],
   ts ~ Eval (Zip ds (Eval (Zip os ls))),
   os ~ Eval (Replicate (Eval (Rank ds)) 0),
   ls ~ Eval (Map (Flip (Fcf.-) 1) (Eval (TakeDims ds (Eval (AsSingleton s))))),
   si ~ Eval (ReplaceDims ds ls (Eval (AsSingleton s))),
   ds ~ Eval (Map Fst ts),
   ls ~ Eval (Map Snd (Eval (Map Snd ts))),
   os ~ Eval (Map Fst (Eval (Map Snd ts))),
   sl ~ Eval (DeleteDims ds (Eval (AsSingleton s)))
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
    HasShape s) => g -> (a,a) -> m (Array s a)
uniform g r = do
  v <- V.replicateM (S.size (shapeOf @s)) (uniformRM r g)
  pure $ array v

-- | Inverse of a square matrix.
--
-- >>> e = array @[3,3] @Double [4,12,-16,12,37,-43,-16,-43,98]
-- >>> pretty (inverse e)
-- [[49.36111111111111,-13.555555555555554,2.1111111111111107],
--  [-13.555555555555554,3.7777777777777772,-0.5555555555555555],
--  [2.1111111111111107,-0.5555555555555555,0.1111111111111111]]
--
-- > D.mult (D.inverse a) a == a
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
invtri :: forall a n. (KnownNat n, ExpField a, Eq a) => Array '[n, n] a -> Array '[n, n] a
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

