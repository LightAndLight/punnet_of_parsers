module Main where

import Test.Hspec (hspec)

import Test.One (oneTests)
import Test.Two (twoTests)

main :: IO ()
main =
  hspec $ do
    oneTests
    twoTests
