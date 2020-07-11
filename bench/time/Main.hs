module Main where

import Criterion.Main (defaultMain)

import Bench.One (oneBenchs)
import Bench.Two (twoBenchs)
import Bench.Three (threeBenchs)
import Bench.Four (fourBenchs)
import Bench.Attoparsec (attoparsecBenchs)
import Bench.Sage (sageBenchs)

main :: IO ()
main =
  defaultMain
  [ oneBenchs
  , twoBenchs
  , threeBenchs
  , fourBenchs
  , attoparsecBenchs
  , sageBenchs
  ]
