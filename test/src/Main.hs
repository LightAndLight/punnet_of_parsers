module Main where

import Test.Hspec (hspec)

import Test.One (oneTests)
import Test.Two (twoTests)
import Test.Three (threeTests)
import Test.Four (fourTests)
import Test.Five (fiveTests)
import Test.Six (sixTests)
import Test.Seven (sevenTests)

main :: IO ()
main =
  hspec $ do
    oneTests
    twoTests
    threeTests
    fourTests
    fiveTests
    sixTests
    sevenTests
