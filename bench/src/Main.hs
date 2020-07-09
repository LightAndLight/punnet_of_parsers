module Main where

import Criterion.Main (defaultMain)

import Bench.One (oneBenchs)
import Bench.Two (twoBenchs)
import Bench.Attoparsec (attoparsecBenchs)

main :: IO ()
main =
  defaultMain
  [ oneBenchs
  , twoBenchs
  , attoparsecBenchs
  ]
