module Bench.Three (threeBenchs, threeWeighs) where

import Criterion.Main (Benchmark)
import Weigh (Weigh)

import Bench.Common (commonBenchs, commonWeighs)
import qualified Bench.Common as Common
import Three (Parser, parse)

expr :: Parser Common.Expr
expr = Common.expr

json :: Parser Common.Json
json = Common.json

threeBenchs :: Benchmark
threeBenchs =
  commonBenchs (parse expr) (parse json) "three"

threeWeighs :: Weigh ()
threeWeighs =
  commonWeighs (parse expr) (parse json) "three"
