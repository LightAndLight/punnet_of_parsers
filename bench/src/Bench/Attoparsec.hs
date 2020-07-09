module Bench.Attoparsec (attoparsecBenchs, attoparsecWeighs) where

import Criterion.Main (Benchmark)

import Bench.Common (commonBenchs, commonWeighs)
import Data.Attoparsec.Text (parseOnly)
import Weigh (Weigh)

attoparsecBenchs :: Benchmark
attoparsecBenchs =
  commonBenchs parseOnly "attoparsec"

attoparsecWeighs :: Weigh ()
attoparsecWeighs =
  commonWeighs parseOnly "attoparsec"
