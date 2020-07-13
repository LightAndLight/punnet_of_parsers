module Main where

import Weigh (mainWith)

import Bench.One (oneWeighs)
import Bench.Two (twoWeighs)
import Bench.Three (threeWeighs)
import Bench.Four (fourWeighs)
import Bench.Five (fiveWeighs)
import Bench.Six (sixWeighs)
import Bench.Seven (sevenWeighs)
import Bench.Attoparsec (attoparsecWeighs)
import Bench.Sage (sageWeighs)

main :: IO ()
main =
  mainWith $ do
    oneWeighs
    twoWeighs
    threeWeighs
    fourWeighs
    fiveWeighs
    sixWeighs
    sevenWeighs
    attoparsecWeighs
    sageWeighs
