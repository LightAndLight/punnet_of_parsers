module Bench.One (oneBenchs, oneWeighs) where

import Criterion.Main (Benchmark)
import Weigh (Weigh)

import Bench.Common (commonBenchs, commonWeighs)
import One (parse)

oneBenchs :: Benchmark
oneBenchs =
  commonBenchs parse "one"

oneWeighs :: Weigh ()
oneWeighs =
  commonWeighs parse "one"
