module Bench.Two (twoBenchs, twoWeighs) where

import Criterion.Main (Benchmark)
import Weigh (Weigh)

import Bench.Common (Expr, commonBenchs, commonWeighs)
import qualified Bench.Common as Common
import Two (Parser, parse)

expr :: Parser Expr
expr = Common.expr

twoBenchs :: Benchmark
twoBenchs =
  commonBenchs (parse expr) "two"

twoWeighs :: Weigh ()
twoWeighs =
  commonWeighs (parse expr) "two"
