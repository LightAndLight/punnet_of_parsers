module Bench.Five (fiveBenchs, fiveWeighs) where

import Criterion.Main (Benchmark)
import Weigh (Weigh)

import Bench.Common (Expr, commonBenchs, commonWeighs)
import qualified Bench.Common as Common
import Five (Parser, parse)

expr :: Parser Expr
expr = Common.expr

fiveBenchs :: Benchmark
fiveBenchs =
  commonBenchs (parse expr) "five"

fiveWeighs :: Weigh ()
fiveWeighs =
  commonWeighs (parse expr) "five"
