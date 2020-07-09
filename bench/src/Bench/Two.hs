module Bench.Two (twoBenchs) where

import Criterion.Main (Benchmark)

import Bench.Common (commonBenchs)
import Two (parse)

twoBenchs :: Benchmark
twoBenchs =
  commonBenchs parse "two"
