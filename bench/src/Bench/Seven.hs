module Bench.Seven (sevenBenchs, sevenWeighs, expr) where

import Criterion.Main (Benchmark)
import Weigh (Weigh)

import Bench.Common (commonBenchs, commonWeighs)
import qualified Bench.Common as Common
import Seven (Parser, parse)

expr :: Parser Common.Expr
expr = Common.expr

json :: Parser Common.Json
json = Common.json

digits :: Parser [Char]
digits = Common.digits

chars :: Parser [Char]
chars = Common.chars

sevenBenchs :: Benchmark
sevenBenchs =
  commonBenchs (parse expr) (parse json) (parse digits) (parse chars) "seven"

sevenWeighs :: Weigh ()
sevenWeighs =
  commonWeighs (parse expr) (parse json) "seven"
