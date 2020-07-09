{-# language DeriveGeneric #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
module Bench.Common (commonBenchs) where

import Control.Applicative ((<|>), many, some)
import Control.DeepSeq (NFData)
import Criterion.Main (Benchmark, bench, bgroup, nf)
import Data.Char (isLower)
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import Text.Parser.Char (CharParsing, char, satisfy, string)
import Text.Parser.Combinators (skipMany)

data Expr = Var String | Lam String Expr | App Expr Expr
  deriving Generic

instance NFData Expr

ident :: CharParsing m => m String
ident = some (satisfy isLower)

expr :: CharParsing m => m Expr
expr =
  lam <|>
  app
  where
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
    ]
  ]
