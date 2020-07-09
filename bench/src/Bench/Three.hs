module Bench.Three (threeBenchs, threeWeighs) where

import Criterion.Main (Benchmark)
import Weigh (Weigh)

import Bench.Common (commonBenchs, commonWeighs)
import Three (parse)

threeBenchs :: Benchmark
threeBenchs =
  commonBenchs parse "three"

threeWeighs :: Weigh ()
threeWeighs =
  commonWeighs parse "three"
