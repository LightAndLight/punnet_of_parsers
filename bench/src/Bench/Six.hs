module Bench.Six (sixBenchs, sixWeighs, expr) where

import Criterion.Main (Benchmark)
import Weigh (Weigh)

import Bench.Common (commonBenchs, commonWeighs)
import qualified Bench.Common as Common
import Six (Parser, parse)

expr :: Parser Common.Expr
expr = Common.expr

json :: Parser Common.Json
json = Common.json

digits :: Parser [Char]
digits = Common.digits

chars :: Parser [Char]
chars = Common.chars

sixBenchs :: Benchmark
sixBenchs =
  commonBenchs (parse expr) (parse json) (parse digits) (parse chars) "six"

sixWeighs :: Weigh ()
sixWeighs =
  commonWeighs (parse expr) (parse json) "six"
