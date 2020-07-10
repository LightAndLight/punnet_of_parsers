module Bench.Five (fiveBenchs, fiveWeighs) where

import Criterion.Main (Benchmark)
import Weigh (Weigh)

import Bench.Common (commonBenchs, commonWeighs)
import qualified Bench.Common as Common
import Five (Parser, parse)

expr :: Parser Common.Expr
expr = Common.expr

json :: Parser Common.Json
json = Common.json

fiveBenchs :: Benchmark
fiveBenchs =
  commonBenchs (parse expr) (parse json) "five"

fiveWeighs :: Weigh ()
fiveWeighs =
  commonWeighs (parse expr) (parse json) "five"
