module Bench.One (oneBenchs, oneWeighs) where

import Criterion.Main (Benchmark)
import Weigh (Weigh)

import Bench.Common (commonBenchs, commonWeighs)
import qualified Bench.Common as Common
import One (Parser, parse)

expr :: Parser Common.Expr
expr = Common.expr

json :: Parser Common.Json
json = Common.json

oneBenchs :: Benchmark
oneBenchs =
  commonBenchs (parse expr) (parse json) "one"

oneWeighs :: Weigh ()
oneWeighs =
  commonWeighs (parse expr) (parse json) "one"
