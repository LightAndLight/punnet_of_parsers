module Bench.Attoparsec (attoparsecBenchs, attoparsecWeighs) where

import Criterion.Main (Benchmark)

import Data.Attoparsec.Text (Parser, parseOnly)
import Weigh (Weigh)

import Bench.Common (commonBenchs, commonWeighs)
import qualified Bench.Common as Common

expr :: Parser Common.Expr
expr = Common.expr

json :: Parser Common.Json
json = Common.json

attoparsecBenchs :: Benchmark
attoparsecBenchs =
  commonBenchs (parseOnly expr) (parseOnly json) "attoparsec"

attoparsecWeighs :: Weigh ()
attoparsecWeighs =
  commonWeighs (parseOnly expr) (parseOnly json) "attoparsec"
