module Bench.Four (fourBenchs) where

import Criterion.Main (Benchmark)

import Bench.Common (commonBenchs)
import Four (parse)

fourBenchs :: Benchmark
fourBenchs =
  commonBenchs parse "four"
