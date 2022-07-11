{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# HLINT ignore "Use fewer LANGUAGE pragmas" #-}
{-# LANGUAGE OverloadedLists #-}

module NumHask.Array.Algorithms
  ( test,
    diag',
  )
where

import Data.List (intercalate, sortBy, sortOn)
import qualified Data.Vector as V
import GHC.Show (Show (..))
import NumHask.Array.Shape
import NumHask.Array.Fixed hiding (mult)
import NumHask.Prelude as P hiding (product)
import Prelude (toRational)
import GHC.TypeLits
import Data.Ord
import Data.Proxy
import Data.Mealy
import Data.Functor.Rep


-- | cholesky decomposition
--
-- > t ==  dot sum (+) (chol t) (transpose (chol t))
chol :: (KnownNat n, ExpField a) => Array '[n,n] a -> Array '[n,n] a
chol a =
  let l =
        tabulate (\[i,j] ->
                    bool
                    (one/index l [j,j] *
                     (index a [i,j] -
                      sum ((\k -> index l [i,k] * index l [j,k]) <$>
                           ([zero..(j - one)]::[Int]))))
                    (sqrt (index a [i,i] -
                        sum ((\k -> index l [j,k] ^ 2) <$>
                             ([zero..(j - one)]::[Int])))) (i==j))
  in l


mpower ::
  (KnownNat s, Subtractive a, Distributive a, P.Ord b, Divisive a, Subtractive b, Integral b) =>
  Array '[s,s] a ->
  b ->
  Array '[s,s] a
mpower x0 y0 =
  case compare y0 zero of
    EQ -> ident
    GT -> f x0 y0
    LT -> error "NYI"
  where
    f x y
      | even y = f (mmult x x) (y `quot` two)
      | y P.== one = x
      | P.otherwise = g (mmult x x) (y `quot` two) x
    g x y z
      | even y = g (mmult x x) (y `quot` two) z
      | y P.== one = mmult x z
      | P.otherwise = g (mmult x x) (y `quot` two) (x * z)

-- | inverse of a triangular matrix
--
-- > ident == dot sum (+) t (invu t)
invt :: forall a n. (KnownNat n, ExpField a, Eq a) => Array '[n,n] a -> Array '[n,n] a
invt a = sum (mpower (-(mmult ti tu)) <$> ([0..(n-1)]::[Int])) `mmult` ti
  where
    d = undiag (diag a)
    ti = fmap (\x -> bool (one/x) zero (x==zero)) d
    tu = a - d
    n = fromIntegral $ natVal @n Proxy

inversion :: (Eq a, KnownNat n, ExpField a) => Array '[n,n] a -> Array '[n,n] a
inversion a = invt (transpose (chol a)) `mmult` invt (chol a)

-- | Extract the diagonal of an array.
--
-- >>> diag (ident :: Array '[3,2] Int)
-- [1, 1]
diag' ::
  forall a s.
  ( HasShape s,
    Additive a
  ) =>
  Array s a ->
  Array s a
diag' a = tabulate go
  where
    go [] = throw (NumHaskException "Rank Underflow")
    go xs@(x:xs') = bool zero (index a xs) (all (x==) xs')


-- | Expand the array to form a diagonal array
--
-- >>> undiag ([1,1,1] :: Array '[3] Int)
--
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
    go xs@(x:xs') = bool zero (index a xs) (all (x==) xs')


test :: IO ()
test = do
  let t = [4,12,-16,12,37,-43,-16,-43,98] :: Array '[3,3] Double
  putStrLn "t"
  print t
  putStrLn "\nchol ="
  print $ chol t
  putStrLn "\ninversion ="
  print $ inversion t
