module Main where

import Test.Hspec (hspec)

import Test.One (oneTests)
import Test.Two (twoTests)
import Test.Three (threeTests)
import Test.Four (fourTests)

main :: IO ()
main =
  hspec $ do
    oneTests
    twoTests
    threeTests
    fourTests
