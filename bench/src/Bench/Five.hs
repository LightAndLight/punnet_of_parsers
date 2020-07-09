module Bench.Five (fiveBenchs, fiveWeighs) where

import Criterion.Main (Benchmark)
import Weigh (Weigh)

import Bench.Common (commonBenchs, commonWeighs)
import Five (parse)

fiveBenchs :: Benchmark
fiveBenchs =
  commonBenchs parse "five"

fiveWeighs :: Weigh ()
fiveWeighs =
  commonWeighs parse "five"
