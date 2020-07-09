{-# language OverloadedStrings #-}
module Bench.Sage (sageBenchs, sageWeighs) where

import Criterion.Main (Benchmark, bench, bgroup, nf)
import Data.Foldable (for_)
import Data.Text (unpack)
import Text.Sage (parse)
import Weigh (Weigh, func', wgroup)

import Bench.Common (expr, commonBenchs, commonWeighs)

sageBenchs :: Benchmark
sageBenchs =
  bgroup "sage"
  [ bgroup "expr"
    [ let
        input = "\\x -> \\y -> x (\\z -> z y) y"
      in
        bench (unpack input) $ nf (parse expr) input
    , let
        input = "\\x -> \\y -> x (\\z -> z y) y (\\x -> (\\y -> ((x y) z) (\\w -> x y w)))"
      in
        bench (unpack input) $ nf (parse expr) input
    ]
  ]

sageWeighs :: Weigh ()
sageWeighs =
  wgroup "sage" . for_ inputs $ \input ->
    func' (unpack input) (parse expr) input
  where
    inputs =
      [ "\\x -> \\y -> x (\\z -> z y) y"
      , "\\x -> \\y -> x (\\z -> z y) y (\\x -> (\\y -> ((x y) z) (\\w -> x y w)))"
      ]
