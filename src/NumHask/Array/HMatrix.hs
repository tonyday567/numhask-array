{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module NumHask.Array.HMatrix
  ( Array(..),
    index,
    mmult,
  ) where

import GHC.Exts (IsList (..))
import GHC.Show (Show (..))
import NumHask.Array.Shape
import NumHask.Prelude as P
import qualified Numeric.LinearAlgebra as H
import qualified Numeric.LinearAlgebra.Devel as H
import qualified Prelude

newtype Array s a = Array {unArray :: H.Matrix a}
  deriving (Show, NFData, Generic)

-- | explicit rather than via Representable
index ::
  forall s a.
  ( HasShape s,
    H.Element a,
    H.Container H.Vector a
  ) =>
  Array s a ->
  [Int] ->
  a
index (Array v) i = H.flatten v `H.atIndex` flatten s i
  where
    s = shapeVal (toShape @s)

instance
  ( Additive a,
    HasShape s,
    H.Container H.Vector a,
    Num a
  ) =>
  Additive (Array s a)
  where

  (+) (Array x1) (Array x2) = Array $ H.add x1 x2

  zero = Array $ H.konst zero (n, m)
    where
      s = shapeVal (toShape @s)
      [n, m] = s

instance
  ( Multiplicative a,
    HasShape s,
    H.Container H.Vector a,
    Num (H.Vector a),
    Num a
  ) =>
  Multiplicative (Array s a)
  where

  (*) (Array x1) (Array x2) = Array $ H.liftMatrix2 (Prelude.*) x1 x2

  one = Array $ H.konst one (n, m)
    where
      s = shapeVal (toShape @s)
      [n, m] = s

type instance Actor (Array s a) = a

instance
  ( HasShape s,
    P.Distributive a,
    CommutativeRing a,
    Semiring a,
    H.Container H.Vector a,
    Num (H.Vector a),
    Num a
  ) =>
  Hilbert (Array s a)
  where
  (<.>) (Array a) (Array b) = H.sumElements $ H.liftMatrix2 (Prelude.*) a b
  {-# INLINE (<.>) #-}

instance
  ( HasShape s,
    Multiplicative a,
    H.Container H.Vector a,
    Num a
  ) =>
  MultiplicativeAction (Array s a)
  where

  (.*) (Array r) s = Array $ H.cmap (* s) r
  {-# INLINE (.*) #-}

  (*.) s (Array r) = Array $ H.cmap (s *) r
  {-# INLINE (*.) #-}

-- | from flat list
instance
  ( HasShape s,
    Additive a,
    H.Element a
  ) =>
  IsList (Array s a)
  where

  type Item (Array s a) = a

  fromList l = Array $ H.reshape n $ H.fromList $ take mn $ l ++ repeat zero
    where
      mn = P.product $ shapeVal (toShape @s)
      s = shapeVal (toShape @s)
      n = Prelude.last s

  toList (Array v) = H.toList $ H.flatten v

-- | fast
mmult ::
  forall m n k a.
  ( KnownNat k,
    KnownNat m,
    KnownNat n,
    HasShape [m, n],
    Ring a,
    H.Numeric a
  ) =>
  Array [m, k] a ->
  Array [k, n] a ->
  Array [m, n] a
mmult (Array x) (Array y) = Array $ x H.<> y

type instance Actor (Array s a) = a
