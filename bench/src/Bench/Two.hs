module Bench.Two (twoBenchs, twoWeighs) where

import Criterion.Main (Benchmark)
import Weigh (Weigh)

import Bench.Common (commonBenchs, commonWeighs)
import Two (parse)

twoBenchs :: Benchmark
twoBenchs =
  commonBenchs parse "two"

twoWeighs :: Weigh ()
twoWeighs =
  commonWeighs parse "two"
