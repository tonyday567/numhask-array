{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import NumHask.Prelude as P
import Test.DocTest

main :: IO ()
main = do
  putStrLn ("NumHask.Array.Fixed DocTest turned on" :: Text)
  doctest ["src/NumHask/Array/Fixed.hs"]
  putStrLn ("NumHask.Array.Shape DocTest turned on" :: Text)
  doctest ["src/NumHask/Array/Shape.hs"]
  putStrLn ("NumHask.Array.Dynamic DocTest turned on" :: Text)
  doctest ["src/NumHask/Array/Dynamic.hs"]
