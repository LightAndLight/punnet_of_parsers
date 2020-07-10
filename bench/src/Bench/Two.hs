module Bench.Two (twoBenchs, twoWeighs) where

import Criterion.Main (Benchmark)
import Weigh (Weigh)

import Bench.Common (commonBenchs, commonWeighs)
import qualified Bench.Common as Common
import Two (Parser, parse)

expr :: Parser Common.Expr
expr = Common.expr

json :: Parser Common.Json
json = Common.json

twoBenchs :: Benchmark
twoBenchs =
  commonBenchs (parse expr) (parse json) "two"

twoWeighs :: Weigh ()
twoWeighs =
  commonWeighs (parse expr) (parse json) "two"
