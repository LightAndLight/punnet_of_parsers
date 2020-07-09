module Bench.One (oneBenchs) where

import Criterion.Main (Benchmark)

import Bench.Common (commonBenchs)
import One (parse)

oneBenchs :: Benchmark
oneBenchs =
  commonBenchs parse "one"
