module Bench.Attoparsec (attoparsecBenchs) where

import Criterion.Main (Benchmark)

import Bench.Common (commonBenchs)
import Data.Attoparsec.Text (parseOnly)

attoparsecBenchs :: Benchmark
attoparsecBenchs =
  commonBenchs parseOnly "attoparsec"
