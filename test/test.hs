{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import GHC.Exts (IsList (..))
import qualified Hedgehog as H
import NumHask.Array.Fixed
import NumHask.Array.Shape
import NumHask.Hedgehog
import NumHask.Prelude as P
import Test.DocTest
import qualified Prelude

genAIntegral :: forall a m r. (HasShape r, H.MonadGen m, Additive a, Bounded a, ToInteger a, FromInteger a) => m (Array (r :: [Nat]) a)
genAIntegral = fromList <$> replicateM (fromIntegral n) integral_
  where
    n = product $ shapeVal $ toShape @r

genARational :: forall a m r. (H.MonadGen m, HasShape r, Field a, Subtractive a, ToRatio a Integer, FromRatio a Integer) => m (Array (r :: [Nat]) a)
genARational = fromList <$> replicateM (fromIntegral n) negUniform
  where
    n = product $ shapeVal $ toShape @r

main :: IO ()
main = do
  putStrLn ("NumHask.Array.Fixed DocTest turned on" :: Text)
  doctest ["src/NumHask/Array/Fixed.hs"]
  putStrLn ("NumHask.Array.Shape DocTest turned on" :: Text)
  doctest ["src/NumHask/Array/Shape.hs"]
  putStrLn ("NumHask.Array.Dynamic DocTest turned on" :: Text)
  doctest ["src/NumHask/Array/Dynamic.hs"]
  bVInt <-
    assertProps
      "Vector Int 6"
      (Prelude.fromInteger 100)
      (genAIntegral :: H.Gen (Vector 6 Int))
      numhaskProps
  bMInt <-
    assertProps
      "Matrix '[3,4] Int"
      (Prelude.fromInteger 100)
      (genAIntegral :: H.Gen (Array '[3, 4] Int))
      numhaskProps
  unless
    (bVInt && bMInt)
    exitFailure

numhaskProps ::
  forall a.
  ( Show a,
    Eq a,
    Subtractive a
  ) =>
  H.Gen a ->
  [(H.PropertyName, H.Property)]
numhaskProps g =
  mconcat $
    (\x -> x g)
      <$> [ isAdditive,
            isSubtractive
          ]
