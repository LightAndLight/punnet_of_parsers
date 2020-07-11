module Bench.Two (twoBenchs, twoWeighs) where

import Criterion.Main (Benchmark)
import Weigh (Weigh)

import Bench.Common (commonBenchs, commonWeighs)
import qualified Bench.Common as Common
import Two (Parser, parse)

expr :: Parser Common.Expr
expr = Common.expr

json :: Parser Common.Json
json = Common.json

digits :: Parser [Char]
digits = Common.digits

chars :: Parser [Char]
chars = Common.chars

twoBenchs :: Benchmark
twoBenchs =
  commonBenchs (parse expr) (parse json) (parse digits) (parse chars) "two"

twoWeighs :: Weigh ()
twoWeighs =
  commonWeighs (parse expr) (parse json) "two"
