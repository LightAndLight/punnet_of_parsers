{-# language DeriveGeneric #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
module Bench.Common (expr, commonBenchs, commonWeighs) where

import Control.Applicative ((<|>), many, some)
import Control.DeepSeq (NFData)
import Criterion.Main (Benchmark, bench, bgroup, nf)
import Data.Char (isLower)
import Data.Foldable (for_)
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import Text.Parser.Char (CharParsing, char, satisfy, string)
import Text.Parser.Combinators (skipMany)
import Weigh (Weigh, func', wgroup)

data Expr = Var String | Lam String Expr | App Expr Expr
  deriving Generic

instance NFData Expr

expr :: CharParsing m => m Expr
expr =
  lam <|>
  app
  where
    ident = some (satisfy isLower)
    spaces = skipMany (char ' ')
    lam = Lam <$ char '\\' <*> ident <* spaces <* string "->" <* spaces <*> expr
    atom =
      (char '(' *> expr <* char ')' <|>
       Var <$> ident
      ) <*
      spaces
    app = foldl App <$> atom <*> many atom

commonBenchs ::
  (CharParsing p, NFData err) =>
  (forall a. NFData a => p a -> Text -> Either err a) ->
  String ->
  Benchmark
commonBenchs parse benchName =
  bgroup benchName
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

commonWeighs ::
  (CharParsing p, NFData err) =>
  (forall a. NFData a => p a -> Text -> Either err a) ->
  String ->
  Weigh ()
commonWeighs parse weighName =
  wgroup weighName . for_ inputs $ \input ->
    func' (unpack input) (parse expr) input
  where
    inputs =
      [ "\\x -> \\y -> x (\\z -> z y) y"
      , "\\x -> \\y -> x (\\z -> z y) y (\\x -> (\\y -> ((x y) z) (\\w -> x y w)))"
      ]
