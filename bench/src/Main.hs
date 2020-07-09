module Main where

import Criterion.Main (defaultMain)

import Bench.One (oneBenchs)
import Bench.Two (twoBenchs)

main :: IO ()
main =
  defaultMain
  [ oneBenchs
  , twoBenchs
  ]
