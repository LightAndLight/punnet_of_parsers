{-# language LambdaCase #-}
module Test.Six (sixTests) where

import qualified Data.Set as Set
import Test.Hspec (Spec)

import Six (Label(..), ParseError(Unexpected), parse)
import Test.Common (commonTests)
import qualified Test.Common as Common

sixTests :: Spec
sixTests =
  commonTests
    parse
    (\pos ls ->
       Unexpected pos $
       Set.map (\case; Common.Eof -> Eof; Common.Char c -> Char c; Common.Name n -> Name n) ls
    )
    "six"
