{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# Language MagicHash #-}
{-# Language CPP #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language ConstraintKinds #-}
{-# Language DataKinds #-}
{-# Language DeriveLift #-}
{-# Language PolyKinds #-}
{-# Language DerivingStrategies #-}
{-# Language FlexibleContexts #-}
{-# Language GADTs #-}
{-# Language MagicHash #-}
{-# Language LambdaCase #-}
{-# Language PatternSynonyms #-}
{-# Language RankNTypes #-}
{-# Language RoleAnnotations #-}
{-# Language ScopedTypeVariables #-}
{-# Language StandaloneDeriving #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}
{-# Language UndecidableInstances #-}
{-# Language Unsafe #-}
{-# Language ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Functions for manipulating shape. The module tends to supply equivalent functionality at type-level and value-level with functions of the same name (except for capitalization).
module NumHask.Array.Shape
  ( -- * Naturals
    withSomeNat,
    valueOf,
    int,
    SNats (..),
    pattern SNats,
    fromSNats,
    KnownNats (..),
    natVals,
    HasShape,
    shapeOf,
    rankOf,
    sizeOf,
    Fin (..),
    safeFin,
    Fins (..),
    toFins,
    flatten,
    shapen,
    isDiag,
    inside,
    Inside,
    ShapeLTE,
    asSingleton,
    AsSingleton,
    asScalar,
    AsScalar,
    GetIndex,
    unsafeGetIndex,
    UnsafeGetIndex,
    rotate,
    type (++),
    Take,
    Drop,
    Reverse,
    Filter,
    rank,
    Rank,
    Range,
    rerank,
    Rerank,
    size,
    Size,
    Min,
    minimum,
    Minimum,
    modifyDim,
    ModifyDim,
    replaceDim,
    ReplaceDim,
    ReplaceDimUncurried,
    incAt,
    IncAt,
    decAt,
    DecAt,
    insertDim,
    InsertDim,
    InsertDimUncurried,
    deleteDim,
    DeleteDim,
    preDeletePositions,
    PreDeletePositions,
    preInsertPositions,
    PreInsertPositions,
    insertDims,
    InsertDims,
    InsertDimsHelper,
    replaceDims,
    ReplaceDims,
    replaceDimsT,
    ReplaceDimsT,
    modifyDims,
    deleteDims,
    DeleteDims,
    takeDims,
    TakeDims,
    dropDims,
    DropDims,
    exclude,
    Exclude,
    concatenate,
    Concatenate,
    ConcatenateOk,
    reorder,
    Reorder,
    ReorderOk,
    squeeze,
    Squeeze,
    expandWindows,
    ExpandWindows,
    indexWindows,
    Fcf.Eval,

    -- * Assertions
    isFin,
    IsFin,
    isFins,
    IsFins,

    -- * index-only operations
    reverseIndex,
    rotateIndex,

    -- * combinators
    EnumFromTo,
    Foldl',
  )
where

import Data.List qualified as List
import Data.Proxy
import Data.Type.Bool hiding (Not)
import Data.Type.Equality
import GHC.TypeLits qualified as L
import Prelude qualified
import NumHask.Prelude as P hiding (Min, Last, minimum)
import Data.Coerce
import Data.Data
import GHC.Arr
import GHC.Exts
import GHC.TypeNats
import GHC.TypeLits (TypeError, ErrorMessage(..))
import Text.Read
import Data.Type.Ord hiding (Min)
import Unsafe.Coerce
import Fcf hiding (type (&&), type (+), type (-), type (++))
import Fcf qualified
import Fcf.Class.Foldable
import Fcf.Data.List
import Control.Monad

-- $setup
-- >>> :m -Prelude
-- >>> :set -XDataKinds
-- >>> :set -XTypeFamilies
-- >>> :set -XFlexibleContexts
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude
-- >>> import NumHask.Array.Shape as S
-- >>> import GHC.TypeNats
-- >>> import Fcf (Eval)

-- | Convert a Nat into an SNat n value, and apply it to a context with both a KnownNat constraint and an SNat input.
--
-- 'withSomeSNat' can be used where there is no KnownNat constraint, but this is almost never given the library design:
--
--
withSomeNat :: forall r. Nat -> (forall (n :: Nat). KnownNat n => SNat n -> r) -> r
withSomeNat n k = withSomeSNat n $
               \(sn :: (SNat n)) -> withKnownNat sn $ k sn

-- | Get the value of a type level Nat.
-- Use with explicit type application
--
-- >>> valueOf @42
-- 42
valueOf :: forall n. (KnownNat n) => Int
valueOf = Prelude.fromIntegral $ natVal (Proxy :: Proxy n)
{-# INLINE valueOf #-}

-- | Get the value of an SNat as an Int.
--
-- >>> int (SNat @42)
-- 42
int :: SNat n -> Int
int = Prelude.fromIntegral . fromSNat

-- | Mimics SNat from GHC.TypeNats
newtype SNats (ns :: [Nat]) = UnsafeSNats [Nat]

instance (KnownNats ns) => Show (SNats ns)
  where
    show s = "SNats @" <> bool "" "'" (length (natVals s) < 2) <> "[" <> mconcat (List.intersperse ", " (show <$> (natVals s))) <> "]"

type role SNats nominal

pattern SNats :: forall ns. () => KnownNats ns => SNats ns
pattern SNats <- (knownNatsInstance -> KnownNatsInstance)
  where SNats = natsSing

fromSNats :: SNats s -> [Nat]
fromSNats (UnsafeSNats s) = s

-- An internal data type that is only used for defining the SNat pattern
-- synonym.
data KnownNatsInstance (ns :: [Nat]) where
  KnownNatsInstance :: KnownNats ns => KnownNatsInstance ns

-- An internal function that is only used for defining the SNat pattern
-- synonym.
knownNatsInstance :: SNats ns -> KnownNatsInstance ns
knownNatsInstance dims = withKnownNats dims KnownNatsInstance

-- | Reflect a list of Nats
class KnownNats (ns :: [Nat]) where
  natsSing :: SNats ns

instance KnownNats '[] where
  natsSing = UnsafeSNats []

instance (KnownNat n, KnownNats s) => KnownNats (n ': s)
  where
    natsSing = UnsafeSNats (fromSNat (SNat :: SNat n) : fromSNats (SNats :: SNats s))

natVals :: forall ns proxy. KnownNats ns => proxy ns -> [Nat]
natVals _ = case natsSing :: SNats ns of
              UnsafeSNats xs -> xs

withKnownNats :: forall ns rep (r :: TYPE rep).
                SNats ns -> (KnownNats ns => r) -> r
withKnownNats = withDict @(KnownNats ns)

{-
-- | The Shape type holds a [Nat] at type level and the equivalent [Int] at value level.
--
-- >>> toShape @[2,3,4]
-- Shape {shapeVal = [2,3,4]}
--
-- A 'Shape' most often represents the dimensions of a hyper-rectangular dense array.
--
-- This could also be called KnownNats
newtype Shape (s :: [Nat]) = Shape {shapeVal :: [Int]} deriving (Show)

class HasShape s where
  toShape :: Shape s

instance HasShape '[] where
  toShape = Shape []

instance (KnownNat n, HasShape s) => HasShape (n : s) where
  toShape = Shape $ Prelude.fromIntegral (natVal (Proxy :: Proxy n)) : shapeVal (toShape :: Shape s)
-}

type HasShape = KnownNats

-- | Supply the value-level of a 'HasShape' as an [Int]
--
-- >>> shapeOf @[2,3,4]
-- [2,3,4]
shapeOf :: forall s. (HasShape s) => [Int]
shapeOf = Prelude.fromIntegral <$> natVals (Proxy :: Proxy s)
{-# INLINE shapeOf #-}

-- | The rank of a 'Shape'.
--
-- >>> rankOf @[2,3,4]
-- 3
rankOf :: forall s. (HasShape s) => Int
rankOf = length (shapeOf @s)
{-# INLINE rankOf #-}

-- | The size of a 'Shape'.
--
-- >>> sizeOf @[2,3,4]
-- 24
sizeOf :: forall s. (HasShape s) => Int
sizeOf = product (shapeOf @s)
{-# INLINE sizeOf #-}

-- | Fin most often represents a (finite) zer-based index for a single dimension (of a multi-dimensioned hyper-rectangular array).
type role Fin nominal
newtype Fin s
  = UnsafeFin
  { fromFin :: Int
  }
  deriving stock (Eq, Ord)

instance Show (Fin n) where
  show (UnsafeFin x) = show x

-- | Construct a Fin safely.
--
-- >>> safeFin 1 :: Maybe (Fin 2)
-- Just 1
--
-- >>> safeFin 2 :: Maybe (Fin 2)
-- Nothing
safeFin :: forall n. (KnownNat n) => Int -> Maybe (Fin n)
safeFin x = bool Nothing (Just (UnsafeFin x)) (x >= 0 && x < valueOf @n)

-- | Fins most often represents (finite) indexes for multiple dimensions (of a multi-dimensioned hyper-rectangular array).
type role Fins nominal
newtype Fins s
  = UnsafeFins
  { fromFins :: [Int]
  }
  deriving stock (Eq, Ord)

instance Show (Fins n) where
  show (UnsafeFins x) = show x

-- | Construct a Fins safely.
--
-- >>> toFins [1,2,3] :: Maybe (Fins [2,3,4])
-- Just [1,2,3]
--
-- >>> toFins [2] :: Maybe (Fins '[2])
-- Nothing
toFins :: forall s. (HasShape s) => [Int] -> Maybe (Fins s)
toFins xs = bool Nothing (Just (UnsafeFins xs)) (inside xs (shapeOf @s))

-- | Number of dimensions
--
-- >>> rank @Int [2,3,4]
-- 3
rank :: [a] -> Int
rank = length
{-# INLINE rank #-}

-- | Number of dimensions
--
-- >>> :k! (Eval (Rank [2,3,4]))
-- (Eval (Rank [2,3,4])) :: Natural
-- = 3
data Rank :: t a -> Exp Natural

type instance Eval (Rank xs) =
  Eval (Length xs)

-- | Enumerate a range of rank n
--
-- FIXME: works in ghci
-- >> :k! Eval (Range 3)
-- ...
-- = [0, 1, 2]
data Range :: Nat -> Exp [Nat]

type instance Eval (Range x) =
  Eval (EnumFromTo 0 (Eval ((Fcf.-) x 1)))

-- | Create a new rank by adding ones to the left, if the new rank is greater, or combining dimensions (from left to right) into rows, if the new rank is lower.
--
-- >>> rerank 4 [2,3,4]
-- [1,2,3,4]
-- >>> rerank 2 [2,3,4]
-- [6,4]
rerank :: Int -> [Int] -> [Int]
rerank r xs =
  replicate (r - r') one
    <> bool [] [product (take (r' - r + 1) xs)] (r <= r')
    <> drop (r' - r + 1) xs
  where
    r' = rank xs

-- | Create a new rank by adding ones to the left, if the new rank is greater, or combining dimensions (from left to right) into rows, if the new rank is lower.
--
-- FIXME: works in ghci
-- >> :k! Eval (Rerank 4 [2,3,4])
-- ...
-- = [1, 2, 3, 4]
-- >> :k! Eval (Rerank 2 [2,3,4])
-- ...
-- = [6, 4]
data Rerank :: Nat -> [Nat] -> Exp [Nat]

type instance Eval (Rerank r xs) =
  If (Eval ((Fcf.>) r (Eval (Rank xs))))
  (Eval (Eval (Replicate (Eval ((Fcf.-) r (Eval (Rank xs)))) 1) Fcf.++ xs))
  (Eval ((Fcf.++) ('[Eval (Size (Eval (Take ((Eval (Rank xs)) - r + 1) xs)))])
    (Eval (Drop (Eval (Rank xs) + 1 - r) xs))))

-- | Total number of elements (if the list is the shape of a hyper-rectangular array).
--
-- >>> size [2,3,4]
-- 24
size :: [Int] -> Int
size [] = 1
size [x] = x
size xs = P.product xs
{-# INLINE size #-}

-- | Total number of elements (if the list is the shape of a hyper-rectangular array).
--
-- >>> :k! (Eval (Size [2,3,4]))
-- (Eval (Size [2,3,4])) :: Natural
-- = 24
data Size :: t Nat -> Exp Nat

type instance Eval (Size xs) = Eval (Foldr (Fcf.*) 1 xs)

-- | convert from n-dim shape list index to a flat index
--
-- >>> flatten [2,3,4] [1,1,1]
-- 17
--
-- >>> flatten [] [1,1,1]
-- 0
flatten :: [Int] -> [Int] -> Int
flatten [] _ = 0
flatten _ [x'] = x'
flatten ns xs = sum $ zipWith (*) xs (drop 1 $ scanr (*) one ns)
{-# INLINE flatten #-}

-- | convert from a flat index to a shape index
--
-- >>> shapen [2,3,4] 17
-- [1,1,1]
shapen :: [Int] -> Int -> [Int]
shapen [] _ = []
shapen [_] x' = [x']
shapen [_, y] x' = let (i, j) = divMod x' y in [i, j]
shapen ns x =
  fst $
    foldr
      ( \a (acc, r) ->
          let (d, m) = divMod r a
           in (m : acc, d)
      )
      ([], x)
      ns
{-# INLINE shapen #-}

isDiag :: (Eq a) => [a] -> Bool
isDiag [] = True
isDiag [_] = True
isDiag [x, y] = x == y
isDiag (x : y : xs) = x == y && isDiag (y : xs)

-- | checks if indices are valid ie they are inside a shape.
--
-- >>> [0,0,0] `inside` [2,3,4]
-- True
-- >>> [1,2,4] `inside` [2,3,4]
-- False
-- >>> [2,1] `inside` [1]
-- False
inside :: [Int] -> [Int] -> Bool
inside xs ds = (rank xs == rank ds) && (List.and $ List.zipWith (\x d -> x >= zero && x < d) xs ds)

-- | checks if indices are valid ie they are of the same rank and inside a shape.
--
-- FIXME:
-- > :k! Eval (Inside [0,0,0] [2,3,4])
-- Eval (Inside [0,0,0] [2,3,4]) :: Bool
-- = True
-- > :k! Eval (Inside [1,2,4] [2,3,4])
-- Eval (Inside [1,2,4] [2,3,4]) :: Bool
-- = False
-- >>> :k! Eval (Inside [2,1] '[1])
-- Eval (Inside [2,1] '[1]) :: Bool
-- = False
data Inside :: t Nat -> t Nat -> Exp Bool

type instance Eval (Inside xs ds) =
  Eval (LiftM2 (Fcf.&&)
    (And =<< (ZipWith (Fcf.<) xs ds))
    (LiftM2 TyEq (Rank xs) (Rank ds)))

-- | Check if a shape is <= another shape (and of the same rank).
-- FIXME:
-- > :k! Eval (ShapeLTE [0,0,0] [2,3,4])
-- Eval (ShapeLTE [0,0,0] [2,3,4]) :: Bool
-- = True
-- > :k! Eval (ShapeLTE [1,2,4] [2,3,4])
-- Eval (ShapeLTE [1,2,4] [2,3,4]) :: Bool
-- = False
-- >>> :k! Eval (ShapeLTE [2,1] '[1])
-- Eval (ShapeLTE [2,1] '[1]) :: Bool
-- = False
data ShapeLTE :: t Nat -> t Nat -> Exp Bool

type instance Eval (ShapeLTE xs ys) =
  Eval (LiftM2 (Fcf.&&)
    (And =<< (ZipWith (Fcf.<=) xs ys))
    (LiftM2 TyEq (Rank xs) (Rank ys)))

-- | Convert a scalar to a dimensioned shape
--
-- >>> asSingleton []
-- [1]
-- >>> asSingleton [2,3,4]
-- [2,3,4]
asSingleton :: [Int] -> [Int]
asSingleton [] = [1]
asSingleton x = x

-- | Convert a scalar to a dimensioned shape
-- >>> :k! Eval (AsSingleton '[])
-- ...
-- = '[1]
-- >>> :k! Eval (AsSingleton [2,3,4])
-- ...
-- = [2, 3, 4]
data AsSingleton :: t Nat -> Exp (t Nat)

type instance Eval (AsSingleton xs) =
  If (xs == '[]) '[1] xs

-- | Convert a (potentially) [1] dimensioned shape to a scalar shape
--
-- >>> asScalar [1]
-- []
-- >>> asScalar [2,3,4]
-- [2,3,4]
asScalar :: [Int] -> [Int]
asScalar [1] = []
asScalar x = x

-- | Convert a (potentially) [1] dimensioned shape to a scalar shape
-- >>> :k! Eval (AsScalar '[1])
-- ...
-- = '[]
-- >>> :k! Eval (AsScalar [2,3,4])
-- ...
-- = [2, 3, 4]
data AsScalar :: t Nat -> Exp (t Nat)

type instance Eval (AsScalar xs) =
  If (xs == '[1]) '[] xs

-- | rotate a list
--
-- >>> rotate 1 [0..3]
-- [1,2,3,0]
-- >>> rotate (-1) [0..3]
-- [3,0,1,2]
rotate :: Int -> [Int] -> [Int]
rotate r xs = drop r' xs <> take r' xs
  where
    r' = r `mod` List.length xs

-- | Get an element at a given index.
--
-- >>> :kind! Eval (GetIndex 2 [2,3,4])
-- ...
-- = Just 4
data GetIndex :: Nat -> [a] -> Exp (Maybe a)
type instance Eval (GetIndex n xs) = GetIndexImpl n xs

type family GetIndexImpl (n :: Nat) (xs :: [k]) where
  GetIndexImpl _ '[] = 'Nothing
  GetIndexImpl 0 (x ': _) = 'Just x
  GetIndexImpl n (_ ': xs) = GetIndexImpl (n - 1) xs

-- | unsafeGetIndex i xs is the i'th element of xs (or error if out-of-bounds)
--
-- >>> unsafeGetIndex 1 [2,3,4]
-- 3
-- >>> unsafeGetIndex 3 [2,3,4]
-- *** Exception: unsafeGetIndex outside bounds
-- ...
unsafeGetIndex :: Int -> [Int] -> Int
unsafeGetIndex 0 (s : _) = s
unsafeGetIndex n (_ : s) = unsafeGetIndex (n - 1) s
unsafeGetIndex _ _ = error "unsafeGetIndex outside bounds"

-- | UnsafeGetIndex i xs is the i'th element of xs (or error if out-of-bounds)
--
-- >>> :k! Eval (UnsafeGetIndex 1 [2,3,4])
-- ...
-- = 3
-- >>> :k! Eval (UnsafeGetIndex 3 [2,3,4])
-- ...
-- = (TypeError ...)
data UnsafeGetIndex :: Nat -> [a] -> Exp a
type instance Eval (UnsafeGetIndex n xs) = Eval (FromMaybe (L.TypeError (L.Text "UnsafeGetIndex out of bounds")) (Eval (GetIndex n xs)))

-- | minimum dimension
--
-- >>> S.minimum []
-- *** Exception: zero-ranked
-- ...
-- >>> S.minimum [2,3,4]
-- 2
minimum :: [Int] -> Int
minimum [] = error "zero-ranked"
minimum [x] = x
minimum (x : xs) = P.min x (minimum xs)

-- | minimum dimension
--
-- >>> :k! Eval (Minimum '[])
-- ...
-- = (TypeError ...)
-- >>> :k! Eval (Minimum [2,3,4])
-- ...
-- = 2
data Minimum :: [a] -> Exp a

type instance Eval (Minimum '[]) = L.TypeError (L.Text "zero ranked")
type instance Eval (Minimum (x ': xs)) =
  Eval (Foldr Min x xs)

data Min :: a -> a -> Exp a

type instance Eval (Min a b) = If (Eval (a Fcf.< b)) a b

-- | delete the i'th dimension
--
-- >>> deleteDim 1 [2, 3, 4]
-- [2,4]
-- >>> deleteDim 2 []
-- []
deleteDim :: Int -> [Int] -> [Int]
deleteDim i s = take i s ++ drop (i + 1) s

-- | delete the i'th dimension
--
-- >>> :k! Eval (DeleteDim 1 [2, 3, 4])
-- ...
-- = [2, 4]
-- >>> :k! Eval (DeleteDim 1 '[])
-- ...
-- = '[]
data DeleteDim :: Nat -> [Nat] -> Exp [Nat]

type instance Eval (DeleteDim i ds) =
  Eval (LiftM2 (Fcf.++) (Take i ds) (Drop (i + 1) ds))

-- | /insertDim d i s/ inserts a new dimension of value i to shape /s/ at position /d/
--
-- >>> insertDim 1 3 [2,4]
-- [2,3,4]
-- >>> insertDim 0 4 []
-- [4]
insertDim :: Int -> Int -> [Int] -> [Int]
insertDim d i s = take d s ++ (i : drop d s)

-- | /insertDim d i s/ inserts a new dimension of value i to shape /s/ at position /d/
--
-- >>> :k! Eval (InsertDim 1 3 [2,4])
-- ...
-- = [2, 3, 4]
-- >>> :k! Eval (InsertDim 0 4 '[])
-- ...
-- = '[4]
data InsertDim :: Nat -> Nat -> [Nat] -> Exp [Nat]

type instance Eval (InsertDim d i ds) =
  Eval (Eval (Take d ds) Fcf.++ (i ': Eval (Drop d ds)))

data InsertDimUncurried :: (Nat,Nat) -> [Nat] -> Exp [Nat]

type instance Eval (InsertDimUncurried xs ds) =
  Eval (Eval (Take (Eval (Fst xs)) ds) Fcf.++ ((Eval (Snd xs)) ': Eval (Drop (Eval (Fst xs)) ds)))

-- | modify an index at a specific dimension. Unmodified if out of bounds.
--
-- >>> modifyDim 0 (+1) [0,1,2]
-- [1,1,2]
modifyDim :: Int -> (Int -> Int) -> [Int] -> [Int]
modifyDim d f xs = take d xs <> (pure . f) (xs List.!! d) <> drop (d + 1) xs

-- | modify an index at a specific dimension. Unmodified if out of bounds.
--
-- >>> :k! Eval (ModifyDim 0 ((Fcf.+) 1) [0,1,2])
-- ...
-- = [1, 1, 2]
data ModifyDim :: Nat -> (Nat -> Exp Nat) -> [Nat] -> Exp [Nat]

type instance Eval (ModifyDim d f ds) =
  Eval (FromMaybe ds =<< (Map (Flip (SetIndex d) ds) =<< (Map f =<< (GetIndex d ds))))

-- | replace an index at a specific dimension.
--
-- >>> replaceDim 0 1 [2,3,4]
-- [1,3,4]
replaceDim :: Int -> Int -> [Int] -> [Int]
replaceDim d x xs = modifyDim d (const x) xs

-- | replace an index at a specific dimension.
--
-- >>> :k! Eval (ReplaceDim 0 1 [2,3,4])
-- ...
-- = [1, 3, 4]
data ReplaceDim :: Nat -> Nat -> [Nat] -> Exp [Nat]

type instance Eval (ReplaceDim d x ds) =
  Eval (SetIndex d x ds)

data ReplaceDimUncurried :: (Nat,Nat) -> [Nat] -> Exp [Nat]

type instance Eval (ReplaceDimUncurried xs ds) =
  Eval (SetIndex (Eval (Fst xs)) (Eval (Snd xs)) ds)

-- | Increment the index at a dimension of a shape by one.
--
-- >>> incAt 1 [2,3,4]
-- [2,4,4]
incAt :: Int -> [Int] -> [Int]
incAt d ds = modifyDim d (+1) ds

-- | Increment the index at a dimension of a shape by one.
--
-- >>> :k! Eval (IncAt 1 [2,3,4])
-- ...
-- = [2, 4, 4]
data IncAt :: Nat -> t Nat -> Exp (t Nat)

type instance Eval (IncAt d ds) =
  Eval (ModifyDim d ((Fcf.+) 1) ds)

-- | Decrement the index at a dimension os a shape by one.
--
-- >>> decAt 1 [2,3,4]
-- [2,2,4]
decAt :: Int -> [Int] -> [Int]
decAt d ds = modifyDim d (\x -> x - 1) ds

-- | Decrement the index at a dimension of a shape by one.
--
-- >>> :k! Eval (DecAt 1 [2,3,4])
-- ...
-- = [2, 2, 4]
data DecAt :: Nat -> t Nat -> Exp (t Nat)

type instance Eval (DecAt d ds) =
  Eval (ModifyDim d (Flip (Fcf.-) 1) ds)

-- | Convert a list of positions that reference deletions according to a final shape to one that references deletions relative to an initial shape.
--
-- To delete the positions [1,2,5] from a list, for example, you need to delete position 1, (arriving at a 4 element list), then position 1, arriving at a 3 element list, and finally position 3.
--
-- >>> preDeletePositions [1,2,5]
-- [1,1,3]
--
-- >>> preDeletePositions [1,2,0]
-- [1,1,0]
--
preDeletePositions :: [Int] -> [Int]
preDeletePositions as = reverse (go as [])
  where
    go [] r = r
    go (x : xs) r = go (decPast x <$> xs) (x : r)
    decPast x y = bool (y - 1) y (y < x)

-- | Convert a list of positions that reference deletions according to a final shape to one that references deletions relative to an initial shape.
--
-- To delete the positions [1,2,5] from a list, for example, you need to delete position 1, (arriving at a 4 element list), then position 1, arriving at a 3 element list, and finally position 3.
--
-- >>> :k! Eval (PreDeletePositions [1,2,5])
-- ...
-- = [1, 1, 3]
--
-- >>> :k! Eval (PreDeletePositions [1,2,0])
-- ...
-- = [1, 1, 0]
data PreDeletePositions :: t Nat -> Exp (t Nat)

type instance Eval (PreDeletePositions xs) =
  Eval (Reverse (Eval (PreDeletePositionsGo xs '[])))

data PreDeletePositionsGo :: t Nat -> t Nat -> Exp (t Nat)

type instance Eval (PreDeletePositionsGo '[] rs) = rs
type instance Eval (PreDeletePositionsGo (x : xs) r) =
  Eval (PreDeletePositionsGo (Eval (Map (DecPast x) xs)) (x : r))

data DecPast :: Nat -> Nat -> Exp Nat

type instance Eval (DecPast x d) =
  If (x + 1 <=? d) (d - 1) d

-- | Convert a list of position that reference insertions according to a final shape to one that references list insertions relative to an initial shape.
--
-- To insert into positions [1,2,0] from a list, starting from a 2 element list, for example, you need to insert at position 0, (arriving at a 3 element list), then position 1, arriving at a 4 element list, and finally position 0.
--
-- > preInsertPositions == reverse . preDeletePositions . reverse
-- >>> preInsertPositions [1,2,5]
-- [1,2,5]
--
-- >>> preInsertPositions [1,2,0]
-- [0,1,0]
preInsertPositions :: [Int] -> [Int]
preInsertPositions = reverse . preDeletePositions . reverse

-- | Convert a list of position that reference insertions according to a final shape to one that references list insertions relative to an initial shape.
--
-- To insert into positions [1,2,0] from a list, starting from a 2 element list, for example, you need to insert at position 0, (arriving at a 3 element list), then position 1, arriving at a 4 element list, and finally position 0.
--
-- > preInsertPositions == reverse . preDeletePositions . reverse
-- >>> :k! Eval (PreInsertPositions [1,2,5])
-- ...
-- = [1, 2, 5]
--
-- >>> :k! Eval (PreInsertPositions [1,2,0])
-- ...
-- = [0, 1, 0]
data PreInsertPositions :: t Nat -> Exp (t Nat)

type instance Eval (PreInsertPositions xs) =
  Eval (Reverse =<< (PreDeletePositions =<< (Reverse xs)))

-- | drop dimensions of a shape according to a list of positions (where position refers to the initial shape)
--
-- >>> deleteDims [1,0] [2, 3, 4]
-- [4]
deleteDims :: [Int] -> [Int] -> [Int]
deleteDims i s = foldl' (flip deleteDim) s (preDeletePositions i)

-- | drop dimensions of a shape according to a list of positions (where position refers to the initial shape)
--
-- >>> :k! Eval (DeleteDims [1,0] [2, 3, 4])
-- ...
-- = '[4]
data DeleteDims :: t Nat -> t Nat -> Exp (t Nat)

type instance Eval (DeleteDims xs ds) =
  Eval (Foldl' (Flip DeleteDim) ds (Eval (PreDeletePositions xs)))

-- | insert a list of dimensions according to dimension,position tuple lists.  Note that the list of positions references the final shape and not the initial shape.
--
-- >>> insertDims [(0,5)] []
-- [5]
-- >>> insertDims [(1,3), (0,2)] [4]
-- [2,3,4]
insertDims :: [(Int,Int)] -> [Int] -> [Int]
insertDims ps ds = foldl' (flip (uncurry insertDim)) ds ps'
  where
    ps' = zip (preInsertPositions $ fmap fst ps) (fmap snd ps)

-- | insert a list of dimensions according to dimension,position tuple lists.  Note that the list of positions references the final shape and not the initial shape.
--
-- >>> :k! Eval (InsertDims '[ '(0,5)] '[])
-- ...
-- = '[5]
-- >>> :k! Eval (InsertDims '[ '(1, 3), '(0, 2)] '[4])
-- ...
-- = [2, 3, 4]
data InsertDims :: t (Nat, Nat) -> t Nat -> Exp (t Nat)

type instance Eval (InsertDims ps ds) =
  Eval (Foldl' (Flip InsertDimUncurried) ds (Eval (InsertDimsHelper ps)))

data InsertDimsHelper :: t (Nat, Nat) -> Exp (t (Nat, Nat))

type instance Eval (InsertDimsHelper ps) =
  Eval (Zip (Eval (PreInsertPositions (Eval (Map Fst ps)))) (Eval (Map Snd ps)))

-- | replace indexes with a new value according to a dimension list.
--
-- >>> replaceDims [0,1] [1,5] [2,3,4]
-- [1,5,4]
--
-- >>> replaceDims [0] [3] []
-- [3]
replaceDims :: [Int] -> [Int] -> [Int] -> [Int]
replaceDims ds xs ns = foldl' (\ns' (d, x) -> replaceDim d x ns') ns (zip ds xs)

-- | replace indexes with a new value according to a dimension list.
--
-- FIXME: Why is this different to value-level result?
--
-- >>> :k! Eval (ReplaceDims [0,1] [1,5] [2,3,4])
-- ...
-- = [1, 5, 4]
--
-- >>> :k! Eval (ReplaceDims '[0] '[3] '[])
-- ...
-- = '[]
data ReplaceDims :: t Nat -> t Nat -> t Nat -> Exp (t Nat)

type instance Eval (ReplaceDims ds xs ns) =
  Eval (Foldl' (Flip ReplaceDimUncurried) ns (Eval (Zip ds xs)))

-- | replace indexes dimension,value tuple list.
--
-- >>> replaceDimsT [(0,1),(1,5)] [2,3,4]
-- [1,5,4]
--
-- >>> replaceDimsT [(0,3)] []
-- [3]
replaceDimsT :: [(Int, Int)] -> [Int] -> [Int]
replaceDimsT ts ns = foldl' (\ns' (d, x) -> replaceDim d x ns') ns ts

-- | replace indexes with a new value according to a dimension list.
--
-- >>> :k! Eval (ReplaceDims [0,1] [1,5] [2,3,4])
-- ...
-- = [1, 5, 4]
--
-- >>> :k! Eval (ReplaceDims '[0] '[3] '[])
-- ...
-- = '[]
data ReplaceDimsT :: t (Nat,Nat) -> t Nat -> Exp (t Nat)

type instance Eval (ReplaceDimsT ts ns) =
  Eval (Foldl' (Flip ReplaceDimUncurried) ns ts)

-- | modify indexes with (separate) functions according to a dimension list.
--
-- >>> modifyDims [0,1] [(+1), (+5)] [2,3,4]
-- [3,8,4]
modifyDims :: [Int] -> [Int -> Int] -> [Int] -> [Int]
modifyDims ds fs ns = foldl' (\ns' (d, f) -> modifyDim d f ns') ns (zip ds fs)

-- | Take dimensions by index.
--
-- >>> takeDims [2,0] [2,3,4]
-- [4,2]
-- >>> takeDims [2] []
-- []
takeDims :: [Int] -> [Int] -> [Int]
takeDims _ [] = []
takeDims i s = (s List.!!) <$> i

-- | Take dimensions by index.
--
-- >>> :k! Eval (TakeDims [2,0] [2,3,4])
-- ...
-- = [4, 2]
-- >>> :k! Eval (TakeDims '[2] '[])
-- ...
-- = '[(TypeError ...)]
data TakeDims :: t Nat -> t Nat -> Exp (t Nat)

type instance Eval (TakeDims xs ds) =
  Eval (Map (Flip UnsafeGetIndex ds) xs)

-- | Compute new size given a drop,n tuple list
--
-- >>> dropDims [(0,1),(2,3)] [2,3,4]
-- [1,3,1]
dropDims :: [(Int, Int)] -> [Int] -> [Int]
dropDims ts ds = replaceDimsT ts' ds
  where
    xs' = zipWith (-) (takeDims (fmap fst ts) ds) (fmap snd ts)
    ts' = zip (fmap fst ts) xs'

-- | Compute new size given a drop,n tuple list
--
-- >>> :k! Eval (DropDims [ '(0,1), '(2,3)] [2,3,4])
-- ...
-- = [1, 3, 1]
data DropDims :: [(Nat,Nat)] -> [Nat] -> Exp [Nat]

type instance Eval (DropDims ts ds) =
  Eval (ReplaceDimsT (Eval (Zip (Eval (Map Fst ts)) (Eval (ZipWith (Fcf.-) (Eval (TakeDims (Eval (Map Fst ts)) ds)) (Eval (Map Snd ts)))))) ds)

-- | Turn a list of included positions for a given rank into a list of excluded positions
--
-- >>> exclude 3 [1,2]
-- [0]
exclude :: Int -> [Int] -> [Int]
exclude r xs = deleteDims xs [0 .. (r - 1)]

-- | Turn a list of included positions for a given rank into a list of excluded positions
--
-- > :k! Eval (Exclude 3 [1,2])
-- ...
-- = '[0]
data Exclude :: Nat -> t Nat -> Exp (t Nat)

type instance Eval (Exclude r xs) =
  Eval (DeleteDims (Eval (EnumFromTo 0 (Eval ((Fcf.-) r 1)))) xs)


-- | concatenate two arrays at dimension i
--
-- Bespoke logic for scalars.
--
-- >>> concatenate 1 [2,3,4] [2,3,4]
-- [2,6,4]
-- >>> concatenate 0 [3] []
-- [4]
-- >>> concatenate 0 [] [3]
-- [4]
-- >>> concatenate 0 [] []
-- [2]
concatenate :: Int -> [Int] -> [Int] -> [Int]
concatenate _ [] [] = [2]
concatenate _ [] [x] = [x + 1]
concatenate _ [x] [] = [x + 1]
concatenate i s0 s1 = take i s0 ++ (unsafeGetIndex i s0 + unsafeGetIndex i s1 : drop (i + 1) s0)

data Concatenate :: Nat -> t Nat -> t Nat -> Exp (t Nat)

type instance Eval (Concatenate i s0 s1) =
  If (Eval (ConcatenateOk i s0 s1))
    (Eval (Eval (Take i s0) ++ (Eval (UnsafeGetIndex i s0) + Eval (UnsafeGetIndex i s1) : Eval (Drop (i + 1) s0))))
    (L.TypeError (L.Text "Concatenate Mis-matched shapes."))

-- | Concatenate is Ok if ranks are the same and the non-indexed portion of the shapes are the same.
data ConcatenateOk :: Nat -> t Nat -> t Nat -> Exp Bool

type instance Eval (ConcatenateOk i s0 s1) =
  Eval (IsFin i (Eval (Rank s0)))
      && Eval (TyEq (Eval (DeleteDim i s0)) (Eval (DeleteDim i s1)))
      && Eval (TyEq (Eval (Rank s0)) (Eval (Rank s1)))

-- | Reorder the dimensions of shape according to a list of positions.
--
-- >>> reorder [2,3,4] [2,0,1]
-- [4,2,3]
reorder :: [Int] -> [Int] -> [Int]
reorder [] _ = []
reorder _ [] = []
reorder s (d : ds) = unsafeGetIndex d s : reorder s ds

-- | Reorder the dimensions of shape according to a list of positions.
--
-- >>> :k! Eval (Reorder [2,3,4] [2,0,1])
-- ...
-- = [4, 2, 3]
data Reorder :: t Nat -> t Nat -> Exp (t Nat)

type instance Eval (Reorder ds xs) =
    If ( Eval (ReorderOk ds xs))
      (Eval (Map (Flip UnsafeGetIndex ds) xs))
      (L.TypeError ('Text "Reorder dimension indices out of bounds"))

data ReorderOk :: t Nat -> t Nat -> Exp Bool

type instance Eval (ReorderOk ds xs) =
  Eval (TyEq (Eval (Rank ds)) (Eval (Rank xs))) &&
  Eval (And =<< Map (Flip IsFin (Eval (Rank ds))) xs)

-- | remove 1's from a list
--
-- >>> squeeze [0,1,2,3]
-- [0,2,3]
squeeze :: (Eq a, Multiplicative a) => [a] -> [a]
squeeze = filter (/= one)

-- | Remove 1's from a list.
--
-- >>> :k! (Eval (Squeeze [0,1,2,3]))
-- (Eval (Squeeze [0,1,2,3])) :: [Natural]
-- = [0, 2, 3]
data Squeeze :: [a] -> Exp [a]

type instance Eval (Squeeze xs) =
  Eval (Filter (Not <=< TyEq 1) xs)


-- | Expanded shape of a windowed array
--
-- >>> expandWindows [2,2] [4,3,2]
-- [3,2,2,2,2]
expandWindows :: [Int] -> [Int] -> [Int]
expandWindows ws ds = List.zipWith (\s' x' -> s' - x' + 1) ds ws <> ws <> List.drop (rank ws) ds

-- | Expanded shape of a windowed array
--
-- >>> :k! Eval (ExpandWindows [2,2] [4,3,2])
-- ...
-- = [3, 2, 2, 2, 2]
data ExpandWindows :: [Nat] -> [Nat] -> Exp [Nat]

type instance Eval (ExpandWindows ws ds) =
  Eval (Eval (ZipWith (Fcf.-) (Eval (Map ((Fcf.+) 1) ds)) ws) ++ Eval (ws ++ Eval (Drop (Eval (Rank ws)) ds)))

-- | Index into windows of an expanded windowed array, given a rank ofthe windows.
--
-- >>> indexWindows 2 [0,1,2,1,1]
-- [2,2,1]
indexWindows :: Int -> [Int] -> [Int]
indexWindows r ds = List.zipWith (+) (List.take r ds) (List.take r (List.drop r ds)) <> List.drop (r + r) ds

-- | Check if i is a valid Fin (aka in-bounds index of a dimension)
--
-- >>> isFin 0 2
-- True
-- >>> isFin 2 2
-- False
isFin :: Int -> Int -> Bool
isFin i d = (zero <= i && i + one <= d)

-- | Check if i is a valid Fin (aka in-bounds index of a dimension)
--
-- >>> :k! Eval (IsFin 0 2)
-- ...
-- = True
-- >>> :k! Eval (IsFin 2 2)
-- ...
-- = False
data IsFin :: Nat -> Nat -> Exp Bool

type instance Eval (IsFin x d) =
  Eval ((Fcf.<) x d)

-- | Check if i is a valid Fins (aka in-bounds index of a Shape)
--
-- >>> isFins [0,1] [2,2]
-- True
-- >>> isFins [0,1] [2,1]
-- False
isFins :: [Int] -> [Int] -> Bool
isFins xs ds = and $ zipWith isFin xs ds

-- | Check if i is a valid Fins (aka in-bounds index of a Shape)
--
-- >>> :k! Eval (IsFins [0,1] [2,2])
-- ...
-- = True
-- >>> :k! Eval (IsFins [0,1] [2,1])
-- ...
-- = False
data IsFins :: [Nat] -> [Nat] -> Exp Bool

type instance Eval (IsFins xs ds) =
  Eval (And (Eval (ZipWith IsFin xs ds)))

-- | reverse an index along specific dimensions.
--
-- >>> reverseIndex [0] [2,3,4] [0,1,2]
-- [1,1,2]
reverseIndex :: [Int] -> [Int] -> [Int] -> [Int]
reverseIndex ds ns xs = fmap (\(i, x, n) -> bool x (n - 1 - x) (i `elem` ds)) (zip3 [0 ..] xs ns)

-- | rotate an index along specific dimensions.
--
-- >>> rotateIndex [(0,1)] [2,3,4] [0,1,2]
-- [1,1,2]
rotateIndex :: [(Int, Int)] -> [Int] -> [Int] -> [Int]
rotateIndex rs s xs = foldr (\(d, r) acc -> modifyDim d (\x -> ((x + r) `mod`) (s List.!! d)) acc) xs rs

-- | Enumerate between two Nats
--
-- >>> :k! Eval (EnumFromTo 0 3)
-- ...
-- = [0, 1, 2, 3]
data EnumFromTo :: Nat -> Nat -> Exp [Nat]

type instance Eval (EnumFromTo a b) = Eval (Unfoldr (EnumFromToHelper b) a)

data EnumFromToHelper :: Nat -> Nat -> Exp (Maybe (a, Nat))
type instance Eval (EnumFromToHelper b a) =
  If (Eval (a Fcf.> b))
    'Nothing
    ('Just '(a, a+1))

-- | Left fold.
--
-- >>> :k! Eval (Foldl' (Fcf.+) 0 [1,2,3])
-- ...
-- = 6
data Foldl' :: (b -> a -> Exp b) -> b -> t a -> Exp b

type instance Eval (Foldl' f y '[]) = y
type instance Eval (Foldl' f y (x ': xs)) = Eval (Foldl' f (Eval (f y x)) xs)
