module Bench.Five (fiveBenchs) where

import Criterion.Main (Benchmark)

import Bench.Common (commonBenchs)
import Five (parse)

fiveBenchs :: Benchmark
fiveBenchs =
  commonBenchs parse "five"
