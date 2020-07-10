{-# language OverloadedStrings #-}
module Bench.Sage (sageBenchs, sageWeighs) where

import Criterion.Main (Benchmark, bench, bgroup, nf)
import Data.Foldable (for_)
import Data.Text (unpack)
import Text.Sage (Parser, parse)
import Weigh (Weigh, func', wgroup)

import Bench.Common (commonBenchs, commonWeighs)
import qualified Bench.Common as Common

expr :: Parser s Common.Expr
expr = Common.expr

json :: Parser s Common.Json
json = Common.json

sageBenchs :: Benchmark
sageBenchs =
  commonBenchs (parse expr) (parse json) "sage"

sageWeighs :: Weigh ()
sageWeighs =
  commonWeighs (parse expr) (parse json) "sage"
