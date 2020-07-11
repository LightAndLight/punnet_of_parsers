{-# language DeriveGeneric #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
module Bench.Common (Expr, expr, Json, json, commonBenchs, commonWeighs) where

import Control.Applicative ((<|>), many, some)
import Control.DeepSeq (NFData)
import Criterion.Main (Benchmark, bench, bgroup, nf)
import Data.Char (isDigit, isLower, ord)
import Data.Foldable (foldl', for_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text, unpack)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import Text.Parser.Char (CharParsing, char, satisfy, string, text)
import Text.Parser.Combinators ((<?>), between, sepBy, skipMany)
import Weigh (Weigh, func', wgroup)

data Expr = Var String | Lam String Expr | App Expr Expr
  deriving (Show, Generic)

instance NFData Expr

{-# inline expr #-}
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

data Json
  = JObject (Map Text Json)
  | JArray (Vector Json)
  | JString Text
  | JNumber Int
  | JTrue
  | JFalse
  | JNull
  deriving Generic

instance NFData Json

{-# inline json #-}
json :: CharParsing m => m Json
json =
  object <|>
  array <|>
  jstring <|>
  number <|>
  bool <|>
  jnull
  where
    spaces = skipMany (char ' ')
    object =
      fmap JObject . between (char '{' *> spaces) (char '}' *> spaces) $
      foldl' (\acc (k, v) -> Map.insert (Text.pack k) v acc) mempty <$>
      sepBy
        ((,) <$>
         between (char '"') (char '"' *> spaces) (some (satisfy isLower)) <* char ':' <* spaces <*>
         json
        )
        (char ',' *> spaces)
    array =
      JArray . Vector.fromList <$>
      between (char '[' *> spaces) (char ']' *> spaces) (json `sepBy` (char ',' *> spaces))
    jstring =
      fmap JString . between (char '"') (char '"' *> spaces) $
      Text.pack <$>
      many
        (satisfy (\c -> c /= '\\' && c /= '"') <|>
         char '\\' *> (char '\\' <|> char '"')
        )
    number =
      (\sign ds ->
         JNumber . either id id sign $
         case ds of
           [] -> errorWithoutStackTrace "number: empty list"
           d : rest -> foldl' (\acc e -> 10 * acc + ord e - 48) (ord d - 48) rest
      ) <$>
      (Left negate <$ char '-' <|> pure (Right id)) <*>
      some (satisfy isDigit <?> "digit") <* spaces
    bool =
      JTrue <$ text "true" <* spaces <|>
      JFalse <$ text "false" <* spaces
    jnull = JNull <$ text "null" <* spaces

commonBenchs ::
  (NFData err, Show err) =>
  (Text -> Either err Expr) ->
  (Text -> Either err Json) ->
  String ->
  Benchmark
commonBenchs parseExpr parseJson benchName =
  bgroup benchName
  [ bgroup "expr"
    [ let
        input = "\\x -> \\y -> x (\\z ->    z  y) y"
      in
        bench (unpack input) $ nf parseExpr input
    , let
        input = "\\x -> \\y -> x (\\z -> z y) y (\\x -> (\\y -> ((x y) z) (\\w -> x y w)))"
      in
        bench (unpack input) $ nf parseExpr input
    ]
  , bgroup "json"
    [ let
        input = "{\"anumber\": 123, \"abool\": true, \"anull\": null, \"anarray\": [1, false, null, {}]}"
      in
        bench (unpack input) $ nf parseJson input
    ]
  ]

commonWeighs ::
  NFData err =>
  (Text -> Either err Expr) ->
  (Text -> Either err Json) ->
  String ->
  Weigh ()
commonWeighs parseExpr parseJson weighName =
  wgroup weighName $ do
    wgroup "expr" . for_ exprInputs $ \input ->
      func' (unpack input) parseExpr input
    wgroup "json" . for_ jsonInputs $ \input ->
      func' (unpack input) parseJson input
  where
    exprInputs =
      [ "\\x -> \\y -> x (\\z ->    z  y) y"
      , "\\x -> \\y -> x (\\z -> z y) y (\\x -> (\\y -> ((x y) z) (\\w -> x y w)))"
      ]
    jsonInputs =
      [ "{\"anumber\": 123, \"abool\": true, \"anull\": null, \"anarray\": [1, false, null, {}]}"
      ]
