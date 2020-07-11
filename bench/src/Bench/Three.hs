module Bench.Three (threeBenchs, threeWeighs, expr) where

import Criterion.Main (Benchmark)
import Weigh (Weigh)

import Bench.Common (commonBenchs, commonWeighs)
import qualified Bench.Common as Common
import Three (Parser, parse)

expr :: Parser Common.Expr
expr = Common.expr

json :: Parser Common.Json
json = Common.json

digits :: Parser [Char]
digits = Common.digits

chars :: Parser [Char]
chars = Common.chars

threeBenchs :: Benchmark
threeBenchs =
  commonBenchs (parse expr) (parse json) (parse digits) (parse chars) "three"

threeWeighs :: Weigh ()
threeWeighs =
  commonWeighs (parse expr) (parse json) "three"
