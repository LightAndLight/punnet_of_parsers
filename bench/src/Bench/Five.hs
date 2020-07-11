module Bench.Five (fiveBenchs, fiveWeighs, expr) where

import Criterion.Main (Benchmark)
import Weigh (Weigh)

import Bench.Common (commonBenchs, commonWeighs)
import qualified Bench.Common as Common
import Five (Parser, parse)

expr :: Parser Common.Expr
expr = Common.expr

json :: Parser Common.Json
json = Common.json

digits :: Parser [Char]
digits = Common.digits

chars :: Parser [Char]
chars = Common.chars

fiveBenchs :: Benchmark
fiveBenchs =
  commonBenchs (parse expr) (parse json) (parse digits) (parse chars) "five"

fiveWeighs :: Weigh ()
fiveWeighs =
  commonWeighs (parse expr) (parse json) "five"
