{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -Wall #-}

-- | Functions for manipulating shape. The module tends to supply equivalent functionality at type-level and value-level with functions of the same name (except for capitalization).
module NumHask.Array.Shape
  ( Shape (..),
    HasShape (..),
    type (++),
    type (!!),
    Take,
    Drop,
    Reverse,
    ReverseGo,
    Filter,
    rank,
    Rank,
    ranks,
    Ranks,
    size,
    Size,
    dimension,
    Dimension,
    flatten,
    shapen,
    minimum,
    Minimum,
    checkIndex,
    CheckIndex,
    checkIndexes,
    CheckIndexes,
    addIndex,
    AddIndex,
    dropIndex,
    DropIndex,
    posRelative,
    PosRelative,
    PosRelativeGo,
    addIndexes,
    AddIndexes,
    AddIndexesGo,
    dropIndexes,
    DropIndexes,
    takeIndexes,
    TakeIndexes,
    exclude,
    Exclude,
    concatenate',
    Concatenate,
    CheckConcatenate,
    Insert,
    CheckInsert,
    reorder',
    Reorder,
    CheckReorder,
    squeeze',
    Squeeze,
    incAt,
    decAt,
    KnownNats (..),
    KnownNatss (..),
  )
where

import Data.List ((!!))
import Data.Type.Bool
import GHC.TypeLits as L
import NumHask.Prelude as P hiding (Last, minimum)

-- | The Shape type holds a [Nat] at type level and the equivalent [Int] at value level.
-- Using [Int] as the index for an array nicely represents the practical interests and constraints downstream of this high-level API: densely-packed numbers (reals or integrals), indexed and layered.
newtype Shape (s :: [Nat]) = Shape {shapeVal :: [Int]} deriving (Show)

class HasShape s where
  toShape :: Shape s

instance HasShape '[] where
  toShape = Shape []

instance (KnownNat n, HasShape s) => HasShape (n : s) where
  toShape = Shape $ fromInteger (natVal (Proxy :: Proxy n)) : shapeVal (toShape :: Shape s)

-- | Number of dimensions
rank :: [a] -> Int
rank = length
{-# INLINE rank #-}

type family Rank (s :: [a]) :: Nat where
  Rank '[] = 0
  Rank (_ : s) = Rank s + 1

-- | The shape of a list of element indexes
ranks :: [[a]] -> [Int]
ranks = fmap rank
{-# INLINE ranks #-}

type family Ranks (s :: [[a]]) :: [Nat] where
  Ranks '[] = '[]
  Ranks (x : xs) = Rank x : Ranks xs

-- | Number of elements
size :: [Int] -> Int
size [] = 1
size [x] = x
size xs = P.product xs
{-# INLINE size #-}

type family Size (s :: [Nat]) :: Nat where
  Size '[] = 1
  Size (n : s) = n L.* Size s

-- | convert from n-dim shape index to a flat index
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

-- | /checkIndex i n/ checks if /i/ is a valid index of a list of length /n/
checkIndex :: Int -> Int -> Bool
checkIndex i n = zero <= i && i + one <= n

type family CheckIndex (i :: Nat) (n :: Nat) :: Bool where
  CheckIndex i n =
    If ((0 <=? i) && (i + 1 <=? n)) 'True (L.TypeError ('Text "index outside range"))

-- | /checkIndexes is n/ check if /is/ are valid indexes of a list of length /n/
checkIndexes :: [Int] -> Int -> Bool
checkIndexes is n = all (`checkIndex` n) is

type family CheckIndexes (i :: [Nat]) (n :: Nat) :: Bool where
  CheckIndexes '[] n = 'True
  CheckIndexes (i : is) n = CheckIndex i n && CheckIndexes is n

-- | dimension i is the i'th dimension of a Shape
dimension :: [Int] -> Int -> Int
dimension (s : _) 0 = s
dimension (_ : s) n = dimension s (n - 1)
dimension _ _ = throw (NumHaskException "dimension overflow")

type family Dimension (s :: [Nat]) (i :: Nat) :: Nat where
  Dimension (s : _) 0 = s
  Dimension (_ : s) n = Dimension s (n - 1)
  Dimension _ _ = L.TypeError ('Text "dimension overflow")

-- | minimum value in a list
minimum :: [Int] -> Int
minimum [] = throw (NumHaskException "dimension underflow")
minimum [x] = x
minimum (x : xs) = P.min x (minimum xs)

type family Minimum (s :: [Nat]) :: Nat where
  Minimum '[] = L.TypeError ('Text "zero dimension")
  Minimum '[x] = x
  Minimum (x : xs) = If (x <=? Minimum xs) x (Minimum xs)

type family Take (n :: Nat) (a :: [k]) :: [k] where
  Take 0 _ = '[]
  Take n (x : xs) = x : Take (n - 1) xs

type family Drop (n :: Nat) (a :: [k]) :: [k] where
  Drop 0 xs = xs
  Drop n (_ : xs) = Drop (n - 1) xs

type family Tail (a :: [k]) :: [k] where
  Tail '[] = L.TypeError ('Text "No tail")
  Tail (_ : xs) = xs

type family Init (a :: [k]) :: [k] where
  Init '[] = L.TypeError ('Text "No init")
  Init '[_] = '[]
  Init (x : xs) = x : Init xs

type family Head (a :: [k]) :: k where
  Head '[] = L.TypeError ('Text "No head")
  Head (x : _) = x

type family Last (a :: [k]) :: k where
  Last '[] = L.TypeError ('Text "No last")
  Last '[x] = x
  Last (_ : xs) = Last xs

type family (a :: [k]) ++ (b :: [k]) :: [k] where
  '[] ++ b = b
  (a : as) ++ b = a : (as ++ b)

-- | drop the i'th dimension from a shape
--
-- >>> dropIndex [2, 3, 4] 1
-- [2,4]
dropIndex :: [Int] -> Int -> [Int]
dropIndex s i = take i s ++ drop (i + 1) s

type DropIndex s i = Take i s ++ Drop (i + 1) s

-- | /addIndex s i d/ adds a new dimension to shape /s/ at position /i/
--
-- >>> addIndex [2,4] 1 3
-- [2,3,4]
addIndex :: [Int] -> Int -> Int -> [Int]
addIndex s i d = take i s ++ (d : drop i s)

type AddIndex s i d = Take i s ++ (d : Drop i s)

type Reverse (a :: [k]) = ReverseGo a '[]

type family ReverseGo (a :: [k]) (b :: [k]) :: [k] where
  ReverseGo '[] b = b
  ReverseGo (a : as) b = ReverseGo as (a : b)

-- | convert a list of position that references a final shape to one that references positions relative to an accumulator.  Deletions are from the left and additions are from the right.
--
-- deletions
--
-- >>> posRelative [0,1]
-- [0,0]
--
-- additions
--
-- >>> reverse (posRelative (reverse [1,0]))
-- [0,0]
posRelative :: [Int] -> [Int]
posRelative as = reverse (go [] as)
  where
    go r [] = r
    go r (x : xs) = go (x : r) ((\y -> bool (y - one) y (y < x)) <$> xs)

type family PosRelative (s :: [Nat]) where
  PosRelative s = PosRelativeGo s '[]

type family PosRelativeGo (r :: [Nat]) (s :: [Nat]) where
  PosRelativeGo '[] r = Reverse r
  PosRelativeGo (x : xs) r = PosRelativeGo (DecMap x xs) (x : r)

type family DecMap (x :: Nat) (ys :: [Nat]) :: [Nat] where
  DecMap _ '[] = '[]
  DecMap x (y : ys) = If (y + 1 <=? x) y (y - 1) : DecMap x ys

-- | drop dimensions of a shape according to a list of positions (where position refers to the initial shape)
--
-- >>> dropIndexes [2, 3, 4] [1, 0]
-- [4]
dropIndexes :: [Int] -> [Int] -> [Int]
dropIndexes s i = foldl' dropIndex s (posRelative i)

type family DropIndexes (s :: [Nat]) (i :: [Nat]) where
  DropIndexes s i = DropIndexesGo s (PosRelative i)

type family DropIndexesGo (s :: [Nat]) (i :: [Nat]) where
  DropIndexesGo s '[] = s
  DropIndexesGo s (i : is) = DropIndexesGo (DropIndex s i) is

-- | insert a list of dimensions according to position and dimension lists.  Note that the list of positions references the final shape and not the initial shape.
--
-- >>> addIndexes [4] [1,0] [3,2]
-- [2,3,4]
addIndexes :: () => [Int] -> [Int] -> [Int] -> [Int]
addIndexes as xs = addIndexesGo as (reverse (posRelative (reverse xs)))
  where
    addIndexesGo as' [] _ = as'
    addIndexesGo as' (x : xs') (y : ys') = addIndexesGo (addIndex as' x y) xs' ys'
    addIndexesGo _ _ _ = throw (NumHaskException "mismatched ranks")

type family AddIndexes (as :: [Nat]) (xs :: [Nat]) (ys :: [Nat]) where
  AddIndexes as xs ys = AddIndexesGo as (Reverse (PosRelative (Reverse xs))) ys

type family AddIndexesGo (as :: [Nat]) (xs :: [Nat]) (ys :: [Nat]) where
  AddIndexesGo as' '[] _ = as'
  AddIndexesGo as' (x : xs') (y : ys') = AddIndexesGo (AddIndex as' x y) xs' ys'
  AddIndexesGo _ _ _ = L.TypeError ('Text "mismatched ranks")

-- | take list of dimensions according to position lists.
--
-- >>> takeIndexes [2,3,4] [2,0]
-- [4,2]
takeIndexes :: [Int] -> [Int] -> [Int]
takeIndexes s i = (s !!) <$> i

type family TakeIndexes (s :: [Nat]) (i :: [Nat]) where
  TakeIndexes '[] _ = '[]
  TakeIndexes _ '[] = '[]
  TakeIndexes s (i : is) =
    (s !! i) ': TakeIndexes s is

type family (a :: [k]) !! (b :: Nat) :: k where
  (!!) '[] i = L.TypeError ('Text "Index Underflow")
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
exclude r = dropIndexes [0 .. (r - 1)]

type family Exclude (r :: Nat) (i :: [Nat]) where
  Exclude r i = DropIndexes (EnumerateGo r) i

-- | concatenate
--
-- >>> concatenate' 1 [2,3,4] [2,3,4]
-- [2,6,4]
concatenate' :: Int -> [Int] -> [Int] -> [Int]
concatenate' i s0 s1 = take i s0 ++ (dimension s0 i + dimension s1 i : drop (i + 1) s0)

type Concatenate i s0 s1 = Take i s0 ++ (Dimension s0 i + Dimension s1 i : Drop (i + 1) s0)

type CheckConcatenate i s0 s1 s =
  ( CheckIndex i (Rank s0)
      && DropIndex s0 i == DropIndex s1 i
      && Rank s0 == Rank s1
  )
    ~ 'True

type CheckInsert d i s =
  (CheckIndex d (Rank s) && CheckIndex i (Dimension s d)) ~ 'True

type Insert d s = Take d s ++ (Dimension s d + 1 : Drop (d + 1) s)

-- | /incAt d s/ increments the index at /d/ of shape /s/ by one.
incAt :: Int -> [Int] -> [Int]
incAt d s = take d s ++ (dimension s d + 1 : drop (d + 1) s)

-- | /decAt d s/ decrements the index at /d/ of shape /s/ by one.
decAt :: Int -> [Int] -> [Int]
decAt d s = take d s ++ (dimension s d - 1 : drop (d + 1) s)

-- | /reorder' s i/ reorders the dimensions of shape /s/ according to a list of positions /i/
--
-- >>> reorder' [2,3,4] [2,0,1]
-- [4,2,3]
reorder' :: [Int] -> [Int] -> [Int]
reorder' [] _ = []
reorder' _ [] = []
reorder' s (d : ds) = dimension s d : reorder' s ds

type family Reorder (s :: [Nat]) (ds :: [Nat]) :: [Nat] where
  Reorder '[] _ = '[]
  Reorder _ '[] = '[]
  Reorder s (d : ds) = Dimension s d : Reorder s ds

type family CheckReorder (ds :: [Nat]) (s :: [Nat]) where
  CheckReorder ds s =
    If
      ( Rank ds == Rank s
          && CheckIndexes ds (Rank s)
      )
      'True
      (L.TypeError ('Text "bad dimensions"))
      ~ 'True

-- | remove 1's from a list
squeeze' :: (Eq a, Num a) => [a] -> [a]
squeeze' = filter (/= 1)

type family Squeeze (a :: [Nat]) where
  Squeeze '[] = '[]
  Squeeze a = Filter '[] a 1

type family Filter (r :: [Nat]) (xs :: [Nat]) (i :: Nat) where
  Filter r '[] _ = Reverse r
  Filter r (x : xs) i = Filter (If (x == i) r (x : r)) xs i

-- unused but useful type-level functions

type family Sort (xs :: [k]) :: [k] where
  Sort '[] = '[]
  Sort (x ': xs) = (Sort (SFilter 'FMin x xs) ++ '[x]) ++ Sort (SFilter 'FMax x xs)

data Flag = FMin | FMax

type family Cmp (a :: k) (b :: k) :: Ordering

type family SFilter (f :: Flag) (p :: k) (xs :: [k]) :: [k] where
  SFilter f p '[] = '[]
  SFilter 'FMin p (x ': xs) = If (Cmp x p == 'LT) (x ': SFilter 'FMin p xs) (SFilter 'FMin p xs)
  SFilter 'FMax p (x ': xs) = If (Cmp x p == 'GT || Cmp x p == 'EQ) (x ': SFilter 'FMax p xs) (SFilter 'FMax p xs)

type family Zip lst lst' where
  Zip lst lst' = ZipWith '(,) lst lst' -- Implemented as TF because #11375

type family ZipWith f lst lst' where
  ZipWith f '[] lst = '[]
  ZipWith f lst '[] = '[]
  ZipWith f (l ': ls) (n ': ns) = f l n ': ZipWith f ls ns

type family Fst a where
  Fst '(a, _) = a

type family Snd a where
  Snd '(_, a) = a

type family FMap f lst where
  FMap f '[] = '[]
  FMap f (l ': ls) = f l ': FMap f ls

-- | Reflect a list of Nats
class KnownNats (ns :: [Nat]) where
  natVals :: Proxy ns -> [Int]

instance KnownNats '[] where
  natVals _ = []

instance (KnownNat n, KnownNats ns) => KnownNats (n : ns) where
  natVals _ = fromInteger (natVal (Proxy @n)) : natVals (Proxy @ns)

-- | Reflect a list of list of Nats
class KnownNatss (ns :: [[Nat]]) where
  natValss :: Proxy ns -> [[Int]]

instance KnownNatss '[] where
  natValss _ = []

instance (KnownNats n, KnownNatss ns) => KnownNatss (n : ns) where
  natValss _ = natVals (Proxy @n) : natValss (Proxy @ns)
