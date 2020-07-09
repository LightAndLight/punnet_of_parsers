module Bench.Attoparsec (attoparsecBenchs, attoparsecWeighs) where

import Criterion.Main (Benchmark)

import Data.Attoparsec.Text (Parser, parseOnly)
import Weigh (Weigh)

import Bench.Common (Expr, commonBenchs, commonWeighs)
import qualified Bench.Common as Common

expr :: Parser Expr
expr = Common.expr

attoparsecBenchs :: Benchmark
attoparsecBenchs =
  commonBenchs (parseOnly expr) "attoparsec"

attoparsecWeighs :: Weigh ()
attoparsecWeighs =
  commonWeighs (parseOnly expr) "attoparsec"
