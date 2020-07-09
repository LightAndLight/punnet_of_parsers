module Bench.Four (fourBenchs, fourWeighs) where

import Criterion.Main (Benchmark)
import Weigh (Weigh)

import Bench.Common (commonBenchs, commonWeighs)
import Four (parse)

fourBenchs :: Benchmark
fourBenchs =
  commonBenchs parse "four"

fourWeighs :: Weigh ()
fourWeighs =
  commonWeighs parse "four"
