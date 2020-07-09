{-# language LambdaCase #-}
module Test.Five (fiveTests) where

import qualified Data.Set as Set
import Test.Hspec (Spec)

import Five (Label(..), ParseError(Unexpected), parse)
import Test.Common (commonTests)
import qualified Test.Common as Common

fiveTests :: Spec
fiveTests =
  commonTests
    parse
    (\pos ls ->
       Unexpected pos $
       Set.map (\case; Common.Eof -> Eof; Common.Char c -> Char c; Common.Name n -> Name n) ls
    )
    "five"
