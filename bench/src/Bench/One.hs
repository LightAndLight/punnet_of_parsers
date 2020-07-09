module Bench.One (oneBenchs, oneWeighs) where

import Criterion.Main (Benchmark)
import Weigh (Weigh)

import Bench.Common (Expr, commonBenchs, commonWeighs)
import qualified Bench.Common as Common
import One (Parser, parse)

expr :: Parser Expr
expr = Common.expr

oneBenchs :: Benchmark
oneBenchs =
  commonBenchs (parse expr) "one"

oneWeighs :: Weigh ()
oneWeighs =
  commonWeighs (parse expr) "one"
