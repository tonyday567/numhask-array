{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Prelude as P
import Test.DocTest

main :: IO ()
main = do
  putStrLn "NumHask.Array.Fixed DocTest turned on"
  doctest ["src/NumHask/Array/Fixed.hs"]
  putStrLn "NumHask.Array.Shape DocTest turned on"
  doctest ["src/NumHask/Array/Shape.hs"]
  putStrLn "NumHask.Array.Dynamic DocTest turned on"
  doctest ["src/NumHask/Array/Dynamic.hs"]
