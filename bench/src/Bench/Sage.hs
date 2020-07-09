{-# language OverloadedStrings #-}
module Bench.Sage (sageBenchs, sageWeighs) where

import Criterion.Main (Benchmark, bench, bgroup, nf)
import Data.Foldable (for_)
import Data.Text (unpack)
import Text.Sage (Parser, parse)
import Weigh (Weigh, func', wgroup)

import Bench.Common (Expr, commonBenchs, commonWeighs)
import qualified Bench.Common as Common

expr :: Parser s Expr
expr = Common.expr

sageBenchs :: Benchmark
sageBenchs =
  commonBenchs (parse expr) "sage"

sageWeighs :: Weigh ()
sageWeighs =
  commonWeighs (parse expr) "sage"
