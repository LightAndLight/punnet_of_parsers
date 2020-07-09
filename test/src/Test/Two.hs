{-# language LambdaCase #-}
module Test.Two (twoTests) where

import qualified Data.Set as Set
import Test.Hspec (Spec)

import Two (Label(..), ParseError(Unexpected), parse)
import Test.Common (commonTests)
import qualified Test.Common as Common

twoTests :: Spec
twoTests =
  commonTests
    parse
    (\pos ls ->
       Unexpected pos $
       Set.map (\case; Common.Eof -> Eof; Common.Char c -> Char c; Common.Name n -> Name n) ls
    )
    "two"
