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
    diff,
    imap,

    -- ** Operator generalisers
    rowWise,
    colWise,

    -- ** Single-dimension operators
    take,
    concatenate,
    insert,
    delete,
    append,
    prepend,
    slice,

    -- * Operators
    takeDs,
    takes,
    takeBs,
    dropDs,
    drops,
    dropBs,
    indexes,
    indexes',
    indexesExcept,
    heads,
    lasts,
    tails,
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

    -- ** Expansion
    expand,
    expandr,
    contract,
    dot,
    mult,
    windows,

    -- * Shape manipulations
    reshape,
    reorder,
    squeeze,
    transpose,
    reverses,
    rotate,
    rotates,

    -- * Maths
    uniform,
    invtri,
    inverse,
    chol,

    -- * Shape specializations
    Vector,
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
import GHC.TypeNats
import NumHask.Array.Dynamic qualified as D
import NumHask.Array.Shape hiding (concatenate, rank, size, asScalar, asSingleton, squeeze, rotate, reorder)
import NumHask.Array.Shape qualified as S
import NumHask.Prelude as P hiding (Min, take, diff, zipWith, empty, sequence, toList, length)
import Prettyprinter hiding (dot)
import Data.List qualified as List
import System.Random hiding (uniform)
import System.Random.Stateful hiding (uniform)

-- $setup
--
-- >>> :m -Prelude
-- >>> :set -XDataKinds
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude hiding (empty, diff, take, drop, zipWith)
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

-- | Row-wise difference an array using the supplied function with a lag.
--
-- >> pretty $ diff (Proxy :: Proxy [1,0]) (-) (range @[3,2])
-- [[2,2],
--  [2,2]]
diff ::
  forall lag s s' a b.
  (HasShape lag,
   HasShape s,
   HasShape s',
   s' ~ Eval (ZipWith (Fcf.-) s lag)
  ) =>
   Proxy lag ->
   (a -> a -> b) ->
   Array s a ->
   Array s' b
diff lag f a = zipWith f (dropDs lag a) (dropBs lag a)

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
-- >>> toDynamic $ rowWise indexes' (Proxy :: Proxy [1,0]) a
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
-- >>> toDynamic $ colWise indexes' (Proxy :: Proxy [1,0]) a
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
-- > withSomeNat 2 (\t -> withSomeSNat 1 (\dim -> F.take dim t a))
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
    KnownNat d,
    KnownNat t,
    -- Fin (Rank s) ~ SNat d,
    Eval (SetIndex d (Eval (Min t (Eval (UnsafeGetIndex d s)))) s) ~ s'
  ) =>
  SNat d ->
  SNat t ->
  Array s a ->
  Array s' a
take _ _ a = unsafeBackpermute id a

-- | Concatenate along a dimension.
--
-- >>> shape $ concatenate (Proxy :: Proxy 1) a a
-- [2,6,4]
-- >>> toDynamic $ concatenate (Proxy :: Proxy 0) (toScalar 1) (toScalar 2)
-- UnsafeArray [2] [1,2]
-- >>> toDynamic $ concatenate (Proxy :: Proxy 0) (array @'[1] [0]) (array @'[3] [1..3])
-- UnsafeArray [4] [0,1,2,3]
concatenate ::
  forall a s0 s1 d s.
  ( Eval (Concatenate d (Eval (AsSingleton s0)) (Eval (AsSingleton s1))) ~ s,
    HasShape s0,
    HasShape s1,
    HasShape s,
    HasShape (Eval (AsSingleton s0)),
    KnownNat d
  ) =>
  Proxy d ->
  Array s0 a ->
  Array s1 a ->
  Array s a
concatenate _ a0 a1 = tabulate (go . fromFins)
  where
    go s =
      bool
        (index a0 (UnsafeFins s))
        ( index
            a1
            ( UnsafeFins $ insertDim
                d
                ((s !! d) - (ds0 !! d))
                (deleteDim d s)
            )
        )
        ((s !! d) >= (ds0 !! d))
    ds0 = shape (asSingleton a0)
    d = valueOf @d

-- | Insert along a dimension at a position.
--
-- >>> pretty $ insert (Proxy :: Proxy 2) 0 a (konst @[2,3] 0)
-- [[[0,0,1,2,3],
--   [0,4,5,6,7],
--   [0,8,9,10,11]],
--  [[0,12,13,14,15],
--   [0,16,17,18,19],
--   [0,20,21,22,23]]]
-- >>> toDynamic $ insert (Proxy :: Proxy 0) 0 (toScalar 1) (toScalar 2)
-- UnsafeArray [2] [2,1]
insert ::
  forall s' s si d a.
  (KnownNat d,
   HasShape s,
   HasShape si,
   HasShape s',
   HasShape (Eval (AsSingleton s)),
   HasShape (Eval (AsSingleton si)),
   s' ~ Eval (IncAt d (Eval (AsSingleton s)))
   ) =>
  Proxy d ->
  Int ->
  Array s a ->
  Array si a ->
  Array s' a
insert _ i a b = tabulate go
  where
    go xs
      | xs' !! d == i = index (asSingleton b) (UnsafeFins (S.deleteDim d xs'))
      | xs' !! d < i = index (asSingleton a) (UnsafeFins xs')
      | otherwise = index (asSingleton a) (UnsafeFins (S.decAt d xs'))
      where xs' = fromFins xs
    d = valueOf @d

-- | Delete along a dimension at a position.
--
-- >>> pretty $ delete (Proxy :: Proxy 2) 0 a
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
   KnownNat d,
   s' ~ Eval (DecAt d s)) =>
  Proxy d ->
  Int ->
  Array s a ->
  Array s' a
delete _ i a = unsafeBackpermute (\s -> bool s (S.incAt (valueOf @d) s) (s !! (valueOf @d) < i)) (asSingleton a)

-- | Insert along a dimension at the end.
--
-- >>> pretty $ append (Proxy :: Proxy 2) a (konst @[2,3] 0)
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
    KnownNat d,
    HasShape s,
    HasShape si,
    HasShape s'
  ) =>
  Proxy d ->
  Array s a ->
  Array si a ->
  Array s' a
append d = insert d (valueOf @pos)

-- | Insert along a dimension at the beginning.
--
-- >>> pretty $ prepend (Proxy :: Proxy 2) (konst @[2,3] 0) a
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
    KnownNat pos,
    pos ~ Eval ((Fcf.-) (Eval (UnsafeGetIndex d s)) 1),
    KnownNat d,
    HasShape s,
    HasShape si,
    HasShape s'
  ) =>
  Proxy d ->
  Array si a ->
  Array s a ->
  Array s' a
prepend d a b = insert d 0 b a

-- | Slice along a dimension with the supplied (offset, length).
--
-- >>> pretty $ slice (Proxy :: Proxy 2) 1 (Proxy :: Proxy 2) a
-- [[[1,2],
--   [5,6],
--   [9,10]],
--  [[13,14],
--   [17,18],
--   [21,22]]]
slice ::
  forall a d l s s'.
  (HasShape s,
   HasShape s',
   KnownNat d,
   Eval (SetIndex d l s) ~ s') =>
  Proxy d ->
  Int ->
  Proxy l ->
  Array s a ->
  Array s' a
slice _ o _ a = unsafeBackpermute (S.modifyDim (valueOf @d) (+ o)) a

-- | Takes the top-most elements according to the new dimensions.
--
-- >>> pretty (takeDs @[1,2,2] a)
-- [[[0,1],
--   [4,5]]]
--
-- > takeDs == rowWise take
--
takeDs ::
  forall s' s a.
  ( HasShape s,
    HasShape s',
    Eval (ShapeLTE s' s) ~ 'True
  ) =>
  Array s a ->
  Array s' a
takeDs a = unsafeBackpermute id a

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

-- | Drops the top-most elements across all dimensions.
--
-- >>> pretty $ dropDs (Proxy :: Proxy [1,2,3]) a
-- [[[23]]]
dropDs ::
  forall ds s' s a.
  ( HasShape s,
    HasShape s',
    s' ~ Eval (ZipWith (Fcf.-) s ds)
  ) =>
  Proxy ds ->
  Array s a ->
  Array s' a
dropDs _ a = unsafeBackpermute (List.zipWith (+) start) a
  where
    start = List.zipWith (-) (shapeOf @s) (shapeOf @s')

-- | Drops the top-most elements across all dimensions.
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

-- | Drops the bottom-most elements across all dimensions.
--
-- >>> pretty $ dropBs (Proxy :: Proxy [1,2,3]) a
-- [[[0]]]
dropBs ::
  forall ds s' s a.
  ( HasShape s,
    HasShape s',
    HasShape ds,
    s' ~ Eval (ZipWith (Fcf.-) s ds)
  ) =>
  Proxy ds ->
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

-- | Select by (dimension,index) pairs.
--
-- > pretty $ indexes' (Proxy :: Proxy [ '(0,1), '(1,1)]) a
-- [16,17,18,19]
indexes' ::
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
indexes' _ a = unsafeBackpermute (S.insertDims (List.zip (shapeOf @ds) (shapeOf @xs))) a

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
-- FIXME: Get new shape into the type level.
-- > pretty $ tails (Proxy :: Proxy [0,2]) (Proxy :: Proxy [1,2,3]) a
-- [[[13,14,15],
--   [17,18,19],
--   [21,22,23]]]
tails ::
  forall ds s s' a ls.
  ( HasShape s,
    HasShape ds,
    HasShape s',
    HasShape ls,
    s' ~ Eval (DeleteDims ds s),
    s' ~ Eval (ReplaceDims ds ls s)
  ) =>
  Proxy ds ->
  Proxy ls ->
  Array s a ->
  Array s' a
tails ds ls a = slices ds (replicate (rankOf @ds) 1) ls a
  where
    -- FIXME:
    -- ls = (\i -> shape a !! i - 1) <$> shapeOf @ds

-- | Slice along a dimension with the supplied (offset, length).
--
-- >>> pretty $ slice (Proxy :: Proxy 2) 1 (Proxy :: Proxy 2) a
-- [[[1,2],
--   [5,6],
--   [9,10]],
--  [[13,14],
--   [17,18],
--   [21,22]]]
slices ::
  forall a ds ls s s'.
  (HasShape s,
   HasShape s',
   HasShape ds,
   HasShape ls,
   Eval (ReplaceDims ds ls s) ~ s') =>
  Proxy ds ->
  [Int] ->
  Proxy ls ->
  Array s a ->
  Array s' a
slices _ o _ a = unsafeBackpermute (S.modifyDims (shapeOf @ds) (fmap (+) o)) a

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
-- > pretty $ zips (Proxy :: Proxy [0,1]) (zipWith (,)) a (reverses [0] a)
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
expand f a b = tabulate (\i -> f (index a (UnsafeFins $ List.take r (fromFins i))) (index b (UnsafeFins $ drop r (fromFins i))))
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
expandr f a b = tabulate (\i -> f (index a (UnsafeFins $ drop r (fromFins i))) (index b (UnsafeFins $ List.take r (fromFins i))))
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

-- | <https://en.wikipedia.org/wiki/Vector_(mathematics_and_physics) Wiki Vector>
type Vector s a = Array '[s] a

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

-- | Generate an array of uniform random variates between a range.
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

