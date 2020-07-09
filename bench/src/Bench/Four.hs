module Bench.Four (fourBenchs, fourWeighs) where

import Criterion.Main (Benchmark)
import Weigh (Weigh)

import Bench.Common (Expr, commonBenchs, commonWeighs)
import qualified Bench.Common as Common
import Four (Parser, parse)

expr :: Parser Expr
expr = Common.expr

fourBenchs :: Benchmark
fourBenchs =
  commonBenchs (parse expr) "four"

fourWeighs :: Weigh ()
fourWeighs =
  commonWeighs (parse expr) "four"
