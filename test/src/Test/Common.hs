{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
module Test.Common (Label(..), commonTests) where

import Control.Applicative ((<|>), many, some)
import Data.Char (isLower)
import Data.Set (Set)
import Data.Text (Text)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)

import Text.Parser.Char (CharParsing, anyChar, char, satisfy, string)
import Text.Parser.Combinators (eof, notFollowedBy, skipMany, try)

data Expr = Var String | Lam String Expr | App Expr Expr
  deriving (Eq, Show)

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

data Label
  = Eof
  | Char Char
  | Name String
  deriving (Eq, Ord, Show)

commonTests ::
  (CharParsing p, Eq err, Show err) =>
  (forall a. p a -> Text -> Either err a) ->
  (Int -> Set Label -> err) ->
  String ->
  Spec
commonTests parse unexpected specName =
  describe specName $ do
    it "parse (char 'a') \"a\"" $ do
      let
        p = char 'a'
        input = "a"
        actual = parse p input
        expected = Right 'a'
      actual `shouldBe` expected
    it "parse (char 'a') \"b\"" $ do
      let
        p = char 'a'
        input = "b"
        actual = parse p input
        expected = Left $ unexpected 0 [Char 'a']
      actual `shouldBe` expected
    it "parse (char 'a' <|> char 'b' <|> char 'c') \"a\"" $ do
      let
        p = char 'a' <|> char 'b' <|> char 'c'
        input = "a"
        actual = parse p input
        expected = Right 'a'
      actual `shouldBe` expected
    it "parse (char 'a' <|> char 'b' <|> char 'c') \"b\"" $ do
      let
        p = char 'a' <|> char 'b' <|> char 'c'
        input = "b"
        actual = parse p input
        expected = Right 'b'
      actual `shouldBe` expected
    it "parse (char 'a' <|> char 'b' <|> char 'c') \"c\"" $ do
      let
        p = char 'a' <|> char 'b' <|> char 'c'
        input = "c"
        actual = parse p input
        expected = Right 'c'
      actual `shouldBe` expected
    it "parse (char 'a' <|> char 'b' <|> char 'c') \"d\"" $ do
      let
        p = char 'a' <|> char 'b' <|> char 'c'
        input = "d"
        actual = parse p input
        expected = Left $ unexpected 0 [Char 'a', Char 'b', Char 'c']
      actual `shouldBe` expected
    it "parse (char 'a' *> char 'b' *> char 'c') \"d\"" $ do
      let
        p = char 'a' *> char 'b' *> char 'c'
        input = "d"
        actual = parse p input
        expected = Left $ unexpected 0 [Char 'a']
      actual `shouldBe` expected
    it "parse (char 'a' *> char 'b' *> char 'c') \"a\"" $ do
      let
        p = char 'a' *> char 'b' *> char 'c'
        input = "a"
        actual = parse p input
        expected = Left $ unexpected 1 [Char 'b']
      actual `shouldBe` expected
    it "parse (char 'a' *> char 'b' *> char 'c') \"ab\"" $ do
      let
        p = char 'a' *> char 'b' *> char 'c'
        input = "ab"
        actual = parse p input
        expected = Left $ unexpected 2 [Char 'c']
      actual `shouldBe` expected
    it "parse (char 'a' *> char 'b' *> char 'c') \"abc\"" $ do
      let
        p = char 'a' *> char 'b' *> char 'c'
        input = "abc"
        actual = parse p input
        expected = Right 'c'
      actual `shouldBe` expected
    it "parse (char 'a' <* notFollowedBy (char 'b')) \"ab\"" $ do
      let
        p = char 'a' <* notFollowedBy (char 'b')
        input = "ab"
        actual = parse p input
        expected = Left $ unexpected 1 mempty
      actual `shouldBe` expected
    it "parse (char 'a' <* notFollowedBy (char 'b')) \"ac\"" $ do
      let
        p = char 'a' <* notFollowedBy (char 'b')
        input = "ac"
        actual = parse p input
        expected = Right 'a'
      actual `shouldBe` expected
    it "parse (char 'a' *> char 'b' <|> char 'a' *> char 'c') \"ab\"" $ do
      let
        p = char 'a' *> char 'b' <|> char 'a' *> char 'c'
        input = "ab"
        actual = parse p input
        expected = Right 'b'
      actual `shouldBe` expected
    it "parse (char 'a' *> char 'b' <|> char 'a' *> char 'c') \"ac\"" $ do
      let
        p = char 'a' *> char 'b' <|> char 'a' *> char 'c'
        input = "ac"
        actual = parse p input
        expected = Left $ unexpected 1 [Char 'b']
      actual `shouldBe` expected
    it "parse (try (char 'a' *> char 'b') <|> char 'a' *> char 'c') \"ac\"" $ do
      let
        p = try (char 'a' *> char 'b') <|> char 'a' *> char 'c'
        input = "ac"
        actual = parse p input
        expected = Right 'c'
      actual `shouldBe` expected
    it "parse (string \"->\") \"->\"" $ do
      let
        p = string "->"
        input = "->"
        actual = parse p input
        expected = Right "->"
      actual `shouldBe` expected
    it "parse (skipMany (char ' ') *> string \"->\") \" ->\"" $ do
      let
        p = skipMany (char ' ') *> string "->"
        input = " ->"
        actual = parse p input
        expected = Right "->"
      actual `shouldBe` expected
    it "parse eof \"\"" $ do
      let
        p = eof
        input = ""
        actual = parse p input
        expected = Right ()
      actual `shouldBe` expected
    it "parse eof \"a\"" $ do
      let
        p = eof
        input = "a"
        actual = parse p input
        expected = Left $ unexpected 0 [Eof]
      actual `shouldBe` expected
    it "parse (many anyChar) \"\"" $ do
      let
        p = many anyChar
        input = ""
        actual = parse p input
        expected = Right []
      actual `shouldBe` expected
    it "parse (many anyChar) \"abc\"" $ do
      let
        p = many anyChar
        input = "abc"
        actual = parse p input
        expected = Right ['a', 'b', 'c']
      actual `shouldBe` expected
    it "parse (many $ char 'a') \"aaa\"" $ do
      let
        p = many $ char 'a'
        input = "aaa"
        actual = parse p input
        expected = Right ['a', 'a', 'a']
      actual `shouldBe` expected
    it "parse (some anyChar) \"abc\"" $ do
      let
        p = some anyChar
        input = "abc"
        actual = parse p input
        expected = Right ['a', 'b', 'c']
      actual `shouldBe` expected
    it "parse (some $ char 'a') \"aaa\"" $ do
      let
        p = some $ char 'a'
        input = "aaa"
        actual = parse p input
        expected = Right ['a', 'a', 'a']
      actual `shouldBe` expected
    it "parse (many (char 'a') <* char 'b') \"aaab\"" $ do
      let
        p = many (char 'a') <* char 'b'
        input = "aaab"
        actual = parse p input
        expected = Right ['a', 'a', 'a']
      actual `shouldBe` expected
    it "parse (some (char 'a') <* char 'b') \"aaab\"" $ do
      let
        p = some (char 'a') <* char 'b'
        input = "aaab"
        actual = parse p input
        expected = Right ['a', 'a', 'a']
      actual `shouldBe` expected
    it "parse (many (char 'a') <* char 'b') \"aaac\"" $ do
      let
        p = many (char 'a') <* char 'b'
        input = "aaac"
        actual = parse p input
        expected = Left $ unexpected 3 [Char 'a', Char 'b']
      actual `shouldBe` expected
    it "parse (some (char 'a') <* char 'b') \"aaac\"" $ do
      let
        p = some (char 'a') <* char 'b'
        input = "aaac"
        actual = parse p input
        expected = Left $ unexpected 3 [Char 'a', Char 'b']
      actual `shouldBe` expected
    it "parse expr \"\\x ->   \\y  -> x     (\\z ->  z  y) y\"" $ do
      let
        input = "\\x -> \\y -> x (\\z -> z y) y"
        actual = parse expr input
        expected =
          Right $
          Lam "x" . Lam "y" $
          App
            (App
               (Var "x")
               (Lam "z" $ App (Var "z") (Var "y"))
            )
            (Var "y")
      actual `shouldBe` expected
    it "parse expr \"\\x -> \\y -> x (\\z -> z y) y (\\x -> (\\y -> ((x y) z) (\\w -> x y w)))\"" $ do
      let
        input = "\\x -> \\y -> x (\\z -> z y) y (\\x -> (\\y -> ((x y) z) (\\w -> x y w)))"
        actual = parse expr input
        expected =
          Right $
          Lam "x" . Lam "y" $
          App
            (App
               (App
                 (Var "x")
                 (Lam "z" $ App (Var "z") (Var "y"))
               )
               (Var "y")
            )
            (Lam "x" $ Lam "y" $
             App
               (App (App (Var "x") (Var "y")) (Var "z"))
               (Lam "w" $ App (App (Var "x") (Var "y")) (Var "w"))
            )
      actual `shouldBe` expected
