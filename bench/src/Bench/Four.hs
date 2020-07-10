module Bench.Four (fourBenchs, fourWeighs) where

import Criterion.Main (Benchmark)
import Weigh (Weigh)

import Bench.Common (commonBenchs, commonWeighs)
import qualified Bench.Common as Common
import Four (Parser, parse)

expr :: Parser Common.Expr
expr = Common.expr

json :: Parser Common.Json
json = Common.json

fourBenchs :: Benchmark
fourBenchs =
  commonBenchs (parse expr) (parse json) "four"

fourWeighs :: Weigh ()
fourWeighs =
  commonWeighs (parse expr) (parse json) "four"
