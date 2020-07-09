module Bench.Three (threeBenchs) where

import Criterion.Main (Benchmark)

import Bench.Common (commonBenchs)
import Three (parse)

threeBenchs :: Benchmark
threeBenchs =
  commonBenchs parse "three"
