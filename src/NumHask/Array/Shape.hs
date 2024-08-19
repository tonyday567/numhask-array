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
  ( KnownNats (..),
    KnownNatss (..),
    valueOf,
    Shape (..),
    HasShape (..),
    shapeOf,
    rankOf,
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
    type (!!),
    Take,
    Drop,
    Reverse,
    Filter,
    rank,
    Rank,
    rerank,
    size,
    Size,
    Min,
    minimum,
    Minimum,
    modifyDim,
    ModifyDim,
    replaceDim,
    ReplaceDim,
    incAt,
    IncAt,
    decAt,
    DecAt,
    insertDim,
    InsertDim,
    deleteDim,
    DeleteDim,
    preDeletePositions,
    preInsertPositions,
    PosRelative,
    DecMap,
    insertDims,
    InsertDims,
    PrependDims,
    replaceDims,
    ReplaceDims,
    modifyDims,
    deleteDims,
    DeleteDims,
    takeDims,
    TakeDims,
    exclude,
    Exclude,
    Enumerate,
    EnumerateGo,
    concatenate,
    Concatenate,
    CheckConcatenate,
    Insert,
    CheckInsert,
    reorder,
    Reorder,
    ReorderOk,
    squeeze,
    Squeeze,
    Zip,
    Windows,
    Fcf.Eval,

    -- * Assertions
    checkIndex,
    CheckIndex,

    -- * index-only operations
    reverseIndex,
    rotateIndex,
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
-- >>> import Fcf (Eval)

-- | Get the value of a type level Nat.
-- Use with explicit type application
--

-- >>> valueOf @42
-- 42
valueOf :: forall n. (KnownNat n) => Int
valueOf = Prelude.fromIntegral $ natVal (Proxy :: Proxy n)
{-# INLINE valueOf #-}

-- | The Shape type holds a [Nat] at type level and the equivalent [Int] at value level.
--
-- >>> toShape @[2,3,4]
-- Shape {shapeVal = [2,3,4]}
--
-- A 'Shape' most often represents the dimensions of a hyper-rectangular dense array.
newtype Shape (s :: [Nat]) = Shape {shapeVal :: [Int]} deriving (Show)

class HasShape s where
  toShape :: Shape s

instance HasShape '[] where
  toShape = Shape []

instance (KnownNat n, HasShape s) => HasShape (n : s) where
  toShape = Shape $ Prelude.fromIntegral (natVal (Proxy :: Proxy n)) : shapeVal (toShape :: Shape s)

-- | Supply the value-level of a 'HasShape'
--
-- >>> shapeOf @[2,3,4]
-- [2,3,4]
shapeOf :: forall s. (HasShape s) => [Int]
shapeOf = shapeVal (toShape @s)
{-# INLINE shapeOf #-}

-- | The length of a 'Shape'.
--
-- >>> rankOf @[2,3,4]
-- 3
rankOf :: forall s. (HasShape s) => Int
rankOf = length (shapeVal (toShape @s))
{-# INLINE rankOf #-}

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

-- | /insertDim i d s/ inserts a new dimension to shape /s/ at position /i/
--
-- >>> insertDim 1 3 [2,4]
-- [2,3,4]
-- >>> insertDim 0 4 []
-- [4]
insertDim :: Int -> Int -> [Int] -> [Int]
insertDim i d s = take i s ++ (d : drop i s)

-- | /insertDim i d s/ inserts a new dimension to shape /s/ at position /i/
--
-- >>> :k! Eval (InsertDim 1 3 [2,4])
-- ...
-- = [2, 3, 4]
-- >>> :k! Eval (InsertDim 0 4 '[])
-- ...
-- = '[4]
data InsertDim :: Nat -> Nat -> [Nat] -> Exp [Nat]

type instance Eval (InsertDim i d ds) =
  Eval (Eval (Take i ds) Fcf.++ (d ': Eval (Drop i ds)))

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


-- | Convert a list of position that reference deletions according to a final shape to one that references deletions relative to an initial shape.
--
-- To delete the positions [1,2,5] from a list, for example, you need to delete position 1, (arriving at a 4 element list), then position 1, arriving at a 3 element list, and finally position 3.
--
-- >>> preDeletePositions [1,2,5]
-- [1,1,3]
--
-- >>> preDeletePositions [1,2,0]
-- [1,1,0]
--
-- >>> reverse (preDeletePositions (reverse [1,0]))
-- [0,0]
preDeletePositions :: [Int] -> [Int]
preDeletePositions as = reverse (go [] as)
  where
    go r [] = r
    go r (x : xs) = go (x : r) ((\y -> bool (y - one) y (y < x)) <$> xs)

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

type family PosRelative (s :: [Nat]) where
  PosRelative s = PosRelativeGo s '[]

type family PosRelativeGo (r :: [Nat]) (s :: [Nat]) where
  PosRelativeGo '[] r = Eval (Reverse r)
  PosRelativeGo (x : xs) r = PosRelativeGo (DecMap x xs) (x : r)

type family DecMap (x :: Nat) (ys :: [Nat]) :: [Nat] where
  DecMap _ '[] = '[]
  DecMap x (y : ys) = If (y + 1 <=? x) y (y - 1) : DecMap x ys

-- | drop dimensions of a shape according to a list of positions (where position refers to the initial shape)
--
-- >>> deleteDims [1,0] [2, 3, 4]
-- [4]
deleteDims :: [Int] -> [Int] -> [Int]
deleteDims i s = foldl' (flip deleteDim) s (preDeletePositions i)

type family DeleteDims (i :: [Nat]) (s :: [Nat]) where
  DeleteDims i s = DeleteDimsGo (PosRelative i) s

type family DeleteDimsGo (i :: [Nat]) (s :: [Nat]) where
  DeleteDimsGo '[] s = s
  DeleteDimsGo (i : is) s = DeleteDimsGo is (Eval (DeleteDim i s))

-- | insert a list of dimensions according to position and dimension lists.  Note that the list of positions references the final shape and not the initial shape.
--
-- >>> insertDims [0] [5] []
-- [5]
-- >>> insertDims [1,0] [3,2] [4]
-- [2,3,4]
insertDims :: [Int] -> [Int] -> [Int] -> [Int]
insertDims xs ys as = insertDimsGo (preInsertPositions xs) ys as
  where
    insertDimsGo [] _ as' = as'
    insertDimsGo (x : xs') (y : ys') as' = insertDimsGo xs' ys' (insertDim x y as')
    insertDimsGo _ _ _ = throw (NumHaskException "mismatched ranks")

type family InsertDims (xs :: [Nat]) (ys :: [Nat]) (as :: [Nat]) where
  InsertDims xs ys as = InsertDimsGo (Eval (Reverse (PosRelative (Eval (Reverse xs))))) ys as

type family InsertDimsGo (xs :: [Nat]) (ys :: [Nat]) (as :: [Nat]) where
  InsertDimsGo '[] _ as' = as'
  InsertDimsGo (x : xs') (y : ys') as' = InsertDimsGo xs' ys' (Eval (InsertDim x y as'))
  InsertDimsGo _ _ _ = L.TypeError ('Text "mismatched ranks")

type family PrependDims (ys :: [Nat]) (as :: [Nat]) where
  PrependDims (y : ys) as = PrependDims ys (y : as)

-- | replace indexes with a new value according to a dimension list.
--
-- >>> replaceDims [0,1] [1,5] [2,3,4]
-- [1,5,4]
--
-- >>> replaceDims [0] [3] []
-- [3]
replaceDims :: [Int] -> [Int] -> [Int] -> [Int]
replaceDims ds xs ns = foldl' (\ns' (d, x) -> replaceDim d x ns') ns (zip ds xs)

type family ReplaceDims (ds :: [Nat]) (rs :: [Nat]) (as :: [Nat]) where
  ReplaceDims '[] _ as' = as'
  ReplaceDims (x : xs') (y : ys') as' = ReplaceDims xs' ys' (Eval (InsertDim x y as'))
  ReplaceDims _ _ _ = L.TypeError ('Text "mismatched ranks")

-- | modify indexes with (separate) functions according to a dimension list.
--
-- >>> modifyDims [0,1] [(+1), (+5)] [2,3,4]
-- [3,8,4]
modifyDims :: [Int] -> [Int -> Int] -> [Int] -> [Int]
modifyDims ds fs ns = foldl' (\ns' (d, f) -> modifyDim d f ns') ns (zip ds fs)

-- | take dimensions by index.
--
-- >>> takeDims [2,0] [2,3,4]
-- [4,2]
-- >>> S.takeDims [2] []
-- []
takeDims :: [Int] -> [Int] -> [Int]
takeDims _ [] = []
takeDims i s = (s List.!!) <$> i

type family TakeDims (i :: [Nat]) (s :: [Nat]) where
  TakeDims '[] _ = '[]
  TakeDims _ '[] = '[]
  TakeDims (i : is) s =
    (s !! i) ': TakeDims is s

type family (a :: [k]) !! (b :: Nat) :: k where
  (!!) '[] _ = L.TypeError ('Text "Index Underflow")
  (!!) (x : _) 0 = x
  (!!) (_ : xs) i = (!!) xs (i - 1)

type family Enumerate (n :: Nat) where
  Enumerate n = Reverse (EnumerateGo n)

type family EnumerateGo (n :: Nat) where
  EnumerateGo 0 = '[]
  EnumerateGo n = (n - 1) : EnumerateGo (n - 1)

-- | turn a list of included positions for a given rank into a list of excluded positions
--
-- >>> exclude 3 [1,2]
-- [0]
exclude :: Int -> [Int] -> [Int]
exclude r xs = deleteDims xs [0 .. (r - 1)]

type family Exclude (r :: Nat) (i :: [Nat]) where
  Exclude r i = DeleteDims (EnumerateGo r) i

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

type Concatenate i s0 s1 = Eval (Take i s0) ++ (Eval (UnsafeGetIndex i s0) + Eval (UnsafeGetIndex i s1) : Eval (Drop (i + 1) s0))

type CheckConcatenate i s0 s1 s =
  ( Eval (CheckIndex i (Eval (Rank s0)))
      && DeleteDim i s0 == DeleteDim i s1
      && Rank s0 == Rank s1
  )
    ~ 'True

type CheckInsert d i s =
  (Eval (CheckIndex d (Eval (Rank s))) && Eval (CheckIndex i (Eval (UnsafeGetIndex d s)))) ~ 'True

type Insert d s = Eval (Take d s) ++ (Eval (UnsafeGetIndex d s) + 1 : Eval (Drop (d + 1) s))

-- | /reorder s i/ reorders the dimensions of shape /s/ according to a list of positions /i/
--
-- >>> reorder [2,3,4] [2,0,1]
-- [4,2,3]
reorder :: [Int] -> [Int] -> [Int]
reorder [] _ = []
reorder _ [] = []
reorder s (d : ds) = unsafeGetIndex d s : reorder s ds

data Reorder :: t Nat -> t Nat -> Exp (t Nat)

type instance Eval (Reorder ds xs) =
    If ( Eval (ReorderOk ds xs))
      (Eval (Map (Flip UnsafeGetIndex ds) xs))
      (L.TypeError ('Text "Reorder dimension indices out of bounds"))

data ReorderOk :: t Nat -> t Nat -> Exp Bool

type instance Eval (ReorderOk ds xs) =
  Eval (TyEq (Eval (Rank ds)) (Eval (Rank xs))) &&
  Eval (And =<< Map (Flip CheckIndex (Eval (Rank ds))) xs)

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

-- | Reflect a list of Nats
class KnownNats (ns :: [Nat]) where
  natVals :: Proxy ns -> [Int]

instance KnownNats '[] where
  natVals _ = []

instance (KnownNat n, KnownNats ns) => KnownNats (n : ns) where
  natVals _ = (Prelude.fromIntegral $ natVal (Proxy @n)) : natVals (Proxy @ns)

-- | Reflect a list of list of Nats
class KnownNatss (ns :: [[Nat]]) where
  natValss :: Proxy ns -> [[Int]]

instance KnownNatss '[] where
  natValss _ = []

instance (KnownNats n, KnownNatss ns) => KnownNatss (n : ns) where
  natValss _ = natVals (Proxy @n) : natValss (Proxy @ns)

type family Windows (ws :: [Nat]) (xs :: [Nat]) where
  Windows ws _ = ws

--     c = List.length xs
--     df s = List.zipWith (\s' x' -> s' - x' + 1) s xs <> xs <> List.drop c s

-- | Check if i is a valid index of a dimension of length l
--
-- >>> checkIndex 0 2
-- True
-- >>> checkIndex 2 2
-- False
checkIndex :: Int -> Int -> Bool
checkIndex i n = (zero <= i && i + one <= n)

-- | Check if i is a valid index of a dimension of length l
-- FIXME: rename to In
--
-- >>> :k! Eval (CheckIndex 0 2)
-- ...
-- = True
-- >>> :k! Eval (CheckIndex 2 2)
-- ...
-- = False
data CheckIndex :: Nat -> Nat -> Exp Bool

type instance Eval (CheckIndex x d) =
  Eval ((Fcf.<) x d)

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

data EnumFromTo :: Nat -> Nat -> Exp (t Nat)

type instance Eval (EnumFromTo a b) =
  If (Eval (a Fcf.> b)) '[] (a : Eval (EnumFromTo (a+1) b))
