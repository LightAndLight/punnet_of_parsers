module Bench.Infinity (infinityBenchs, infinityWeighs) where

import Criterion.Main (Benchmark)
import Weigh (Weigh)

import Bench.Common (commonBenchs, commonWeighs)
import qualified Bench.Common as Common
import Infinity (Parser, parse)

expr :: Parser Common.Expr
expr = Common.expr

json :: Parser Common.Json
json = Common.json

infinityBenchs :: Benchmark
infinityBenchs =
  commonBenchs (parse expr) (parse json) "infinity"

infinityWeighs :: Weigh ()
infinityWeighs =
  commonWeighs (parse expr) (parse json) "infinity"
