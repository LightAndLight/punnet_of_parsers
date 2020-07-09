module Bench.Three (threeBenchs, threeWeighs) where

import Criterion.Main (Benchmark)
import Weigh (Weigh)

import Bench.Common (Expr, commonBenchs, commonWeighs)
import qualified Bench.Common as Common
import Three (Parser, parse)

expr :: Parser Expr
expr = Common.expr

threeBenchs :: Benchmark
threeBenchs =
  commonBenchs (parse expr) "three"

threeWeighs :: Weigh ()
threeWeighs =
  commonWeighs (parse expr) "three"
