module Bench.Four (fourBenchs, fourWeighs, expr) where

import Criterion.Main (Benchmark)
import Weigh (Weigh)

import Bench.Common (commonBenchs, commonWeighs)
import qualified Bench.Common as Common
import Four (Parser, parse)

expr :: Parser Common.Expr
expr = Common.expr

json :: Parser Common.Json
json = Common.json

digits :: Parser [Char]
digits = Common.digits

chars :: Parser [Char]
chars = Common.chars

fourBenchs :: Benchmark
fourBenchs =
  commonBenchs (parse expr) (parse json) (parse digits) (parse chars) "four"

fourWeighs :: Weigh ()
fourWeighs =
  commonWeighs (parse expr) (parse json) "four"
