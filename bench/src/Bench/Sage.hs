{-# language OverloadedStrings #-}
module Bench.Sage (sageBenchs, sageWeighs) where

import Criterion.Main (Benchmark)
import Text.Sage (Parser, parse)
import Weigh (Weigh)

import Bench.Common (commonBenchs, commonWeighs)
import qualified Bench.Common as Common

expr :: Parser s Common.Expr
expr = Common.expr

json :: Parser s Common.Json
json = Common.json

digits :: Parser s [Char]
digits = Common.digits

chars :: Parser s [Char]
chars = Common.chars

sageBenchs :: Benchmark
sageBenchs =
  commonBenchs (parse expr) (parse json) (parse digits) (parse chars) "sage"

sageWeighs :: Weigh ()
sageWeighs =
  commonWeighs (parse expr) (parse json) "sage"
