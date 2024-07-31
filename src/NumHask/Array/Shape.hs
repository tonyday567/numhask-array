{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Functions for manipulating shape. The module tends to supply equivalent functionality at type-level and value-level with functions of the same name (except for capitalization).
module NumHask.Array.Shape
  ( KnownNats (..),
    KnownNatss (..),
    valueOf,
    Shape (..),
    HasShape (..),
    shapeOf,
    flatten,
    shapen,
    isDiag,
    inside,
    rotate,
    type (++),
    type (!!),
    Take,
    Drop,
    Reverse,
    Filter,
    rank,
    rerank,
    Rank,
    ranks,
    Ranks,
    size,
    Size,
    indexOf,
    IndexOf,
    minimum,
    Minimum,
    checkIndex,
    CheckIndex,
    checkIndexes,
    CheckIndexes,
    reverseIndex,
    modifyDim,
    rotateIndex,
    insertDim,
    InsertDim,
    deleteDim,
    DeleteDim,
    replaceDim,
    preDeletePositions,
    preInsertPositions,
    PosRelative,
    DecMap,
    insertDims,
    InsertDims,
    replaceDims,
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
    CheckReorder,
    squeeze,
    Squeeze,
    incAt,
    decAt,
  )
where

import Data.List qualified as List
import Data.Proxy
import Data.Type.Bool
import Data.Type.Equality
import GHC.TypeLits as L
import NumHask.Prelude as P hiding (Last, minimum)

-- $setup
-- >>> :m -Prelude
-- >>> :set -XDataKinds
-- >>> :set -XTypeFamilies
-- >>> :set -XFlexibleContexts
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude
-- >>> import NumHask.Array.Shape as S

-- | Get the value of a type level Nat.
-- Use with explicit type application, i.e., @valueOf \@42@
valueOf :: forall n. (KnownNat n) => Int
valueOf = fromIntegral $ natVal (Proxy :: Proxy n)
{-# INLINE valueOf #-}

-- | The Shape type holds a [Nat] at type level and the equivalent [Int] at value level.
-- Using [Int] as the index for an array nicely represents the practical interests and constraints downstream of this high-level API: densely-packed numbers (reals or integrals), indexed and layered.
newtype Shape (s :: [Nat]) = Shape {shapeVal :: [Int]} deriving (Show)

class HasShape s where
  toShape :: Shape s

instance HasShape '[] where
  toShape = Shape []

instance (KnownNat n, HasShape s) => HasShape (n : s) where
  toShape = Shape $ fromInteger (natVal (Proxy :: Proxy n)) : shapeVal (toShape :: Shape s)

shapeOf :: forall s. (HasShape s) => [Int]
shapeOf = shapeVal (toShape @s)
{-# INLINE shapeOf #-}

-- | Number of dimensions
rank :: [a] -> Int
rank = length
{-# INLINE rank #-}

type family Rank (s :: [a]) :: Nat where
  Rank '[] = 0
  Rank (_ : s) = Rank s + 1

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
-- >>> [-1] `inside` [1]
-- False
inside :: [Int] -> [Int] -> Bool
inside i r = List.and $ List.zipWith (\i' r' -> i' >= zero && i' < r') i r

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

-- | indexOf i xs is the i'th element of xs (or zero if out-of-bounds)
--
-- >>> indexOf 1 [2,3,4]
-- 3
indexOf :: Int -> [Int] -> Int
indexOf 0 (s : _) = s
indexOf n (_ : s) = indexOf (n - 1) s
indexOf _ _ = error "indexOf outside bounds"

type family IndexOf (i :: Nat) (xs :: [Nat]) :: Nat where
  IndexOf 0 (xs : _) = xs
  IndexOf n (_ : xs) = IndexOf (n - 1) xs
  IndexOf _ _ = L.TypeError ('Text "indexOf outside bounds")

-- | minimum value in a list
--
-- >>> S.minimum []
-- 0
-- >>> S.minimum [2,3,4]
-- 2
minimum :: [Int] -> Int
minimum [] = 0
minimum [x] = x
minimum (x : xs) = P.min x (minimum xs)

type family Minimum (s :: [Nat]) :: Nat where
  Minimum '[] = L.TypeError ('Text "zero dimension")
  Minimum '[x] = x
  Minimum (x : xs) = If (x <=? Minimum xs) x (Minimum xs)

type family Take (n :: Nat) (a :: [k]) :: [k] where
  Take _ '[] = '[]
  Take 0 _ = '[]
  Take n (x : xs) = x : Take (n - 1) xs

type family Drop (n :: Nat) (a :: [k]) :: [k] where
  Drop _ '[] = '[]
  Drop 0 xs = xs
  Drop n (_ : xs) = Drop (n - 1) xs

type family Init (a :: [k]) :: [k] where
  Init '[] = L.TypeError ('Text "No init")
  Init '[_] = '[]
  Init (x : xs) = x : Init xs

type family Last (a :: [k]) :: k where
  Last '[] = L.TypeError ('Text "No last")
  Last '[x] = x
  Last (_ : xs) = Last xs

type family (a :: [k]) ++ (b :: [k]) :: [k] where
  '[] ++ b = b
  (a : as) ++ b = a : (as ++ b)

-- | delete the i'th dimension
--
-- >>> deleteDim 1 [2, 3, 4]
-- [2,4]
-- >>> deleteDim 2 []
-- []
deleteDim :: Int -> [Int] -> [Int]
deleteDim i s = take i s ++ drop (i + 1) s

type DeleteDim i s = Take i s ++ Drop (i + 1) s

-- | /insertDim i d s/ adds a new dimension to shape /s/ at position /i/
--
-- >>> insertDim 1 3 [2,4]
-- [2,3,4]
-- >>> insertDim 0 4 []
-- [4]
insertDim :: Int -> Int -> [Int] -> [Int]
insertDim i d s = take i s ++ (d : drop i s)

type InsertDim i d s = Take i s ++ (d : Drop i s)

-- | modify an index at a specific dimension. Unmodified if out of bounds.
--
-- >>> modifyDim 0 (+1) [0,1,2]
-- [1,1,2]
modifyDim :: Int -> (Int -> Int) -> [Int] -> [Int]
modifyDim d f xs = take d xs <> (pure . f) (xs List.!! d) <> drop (d + 1) xs

-- | replace an index at a specific dimension.
--
-- >>> replaceDim 0 1 [2,3,4]
-- [1,3,4]
replaceDim :: Int -> Int -> [Int] -> [Int]
replaceDim d x xs = modifyDim d (const x) xs

-- | reverse an index along specific dimensions.
--
-- >>> reverseIndex [0] [2,3,4] [0,1,2]
-- [1,1,2]
reverseIndex :: [Int] -> [Int] -> [Int] -> [Int]
reverseIndex ds ns xs = fmap (\(i, x, n) -> bool x (n - 1 - x) (i `elem` ds)) (zip3 [0 ..] xs ns)

type Reverse (a :: [k]) = ReverseGo a '[]

type family ReverseGo (a :: [k]) (b :: [k]) :: [k] where
  ReverseGo '[] b = b
  ReverseGo (a : as) b = ReverseGo as (a : b)

-- | rotate an index along specific dimensions.
--
-- >>> rotateIndex [(0,1)] [2,3,4] [0,1,2]
-- [1,1,2]
rotateIndex :: [(Int, Int)] -> [Int] -> [Int] -> [Int]
rotateIndex rs s xs = foldr (\(d, r) acc -> modifyDim d (\x -> ((x + r) `mod`) (s List.!! d)) acc) xs rs

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
  PosRelativeGo '[] r = Reverse r
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
  DeleteDimsGo (i : is) s = DeleteDimsGo is (DeleteDim i s)

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
  InsertDims xs ys as = InsertDimsGo (Reverse (PosRelative (Reverse xs))) ys as

type family InsertDimsGo (xs :: [Nat]) (ys :: [Nat]) (as :: [Nat]) where
  InsertDimsGo '[] _ as' = as'
  InsertDimsGo (x : xs') (y : ys') as' = InsertDimsGo xs' ys' (InsertDim x y as')
  InsertDimsGo _ _ _ = L.TypeError ('Text "mismatched ranks")

-- | replace indexes with a new value according to a dimension list.
--
-- >>> replaceDims [0,1] [1,5] [2,3,4]
-- [1,5,4]
--
-- >>> replaceDims [0] [3] []
-- [3]
replaceDims :: [Int] -> [Int] -> [Int] -> [Int]
replaceDims ds xs ns = foldl' (\ns' (d, x) -> replaceDim d x ns') ns (zip ds xs)

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

-- | /checkIndex i n/ checks if /i/ is a valid index of a list of length /n/
--
-- >>> checkIndex 0 0
-- True
-- >>> checkIndex 3 2
-- False
checkIndex :: Int -> Int -> Bool
checkIndex i n = (zero <= i && i + one <= n) || (i == zero && n == zero)

type family CheckIndex (i :: Nat) (n :: Nat) :: Bool where
  CheckIndex i n =
    If ((0 <=? i) && (i + 1 <=? n)) 'True (L.TypeError ('Text "index outside range"))

-- | /checkIndexes is n/ check if /is/ are valid indexes of a list of length /n/
checkIndexes :: [Int] -> Int -> Bool
checkIndexes is n = all (`checkIndex` n) is

type family CheckIndexes (i :: [Nat]) (n :: Nat) :: Bool where
  CheckIndexes '[] _ = 'True
  CheckIndexes (i : is) n = CheckIndex i n && CheckIndexes is n

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
concatenate i s0 s1 = take i s0 ++ (indexOf i s0 + indexOf i s1 : drop (i + 1) s0)

type Concatenate i s0 s1 = Take i s0 ++ (IndexOf i s0 + IndexOf i s1 : Drop (i + 1) s0)

type CheckConcatenate i s0 s1 s =
  ( CheckIndex i (Rank s0)
      && DeleteDim i s0 == DeleteDim i s1
      && Rank s0 == Rank s1
  )
    ~ 'True

type CheckInsert d i s =
  (CheckIndex d (Rank s) && CheckIndex i (IndexOf d s)) ~ 'True

type Insert d s = Take d s ++ (IndexOf d s + 1 : Drop (d + 1) s)

-- | /incAt d s/ increments the index at /d/ of shape /s/ by one.
incAt :: Int -> [Int] -> [Int]
incAt d s = take d s ++ (indexOf d s + 1 : drop (d + 1) s)

-- | /decAt d s/ decrements the index at /d/ of shape /s/ by one.
decAt :: Int -> [Int] -> [Int]
decAt d s = take d s ++ (indexOf d s - 1 : drop (d + 1) s)

-- | /reorder s i/ reorders the dimensions of shape /s/ according to a list of positions /i/
--
-- >>> reorder [2,3,4] [2,0,1]
-- [4,2,3]
reorder :: [Int] -> [Int] -> [Int]
reorder [] _ = []
reorder _ [] = []
reorder s (d : ds) = indexOf d s : reorder s ds

type family Reorder (s :: [Nat]) (ds :: [Nat]) :: [Nat] where
  Reorder '[] _ = '[]
  Reorder _ '[] = '[]
  Reorder s (d : ds) = IndexOf d s : Reorder s ds

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
squeeze :: (Eq a, Multiplicative a) => [a] -> [a]
squeeze = filter (/= one)

type family Squeeze (a :: [Nat]) where
  Squeeze '[] = '[]
  Squeeze a = Filter '[] a 1

type family Filter (r :: [Nat]) (xs :: [Nat]) (i :: Nat) where
  Filter r '[] _ = Reverse r
  Filter r (x : xs) i = Filter (If (x == i) r (x : r)) xs i

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
