{-# language LambdaCase #-}
module Test.Infinity (infinityTests) where

import qualified Data.Set as Set
import Test.Hspec (Spec)

import Infinity (Label(..), ParseError(Unexpected), parse)
import Test.Common (commonTests)
import qualified Test.Common as Common

infinityTests :: Spec
infinityTests =
  commonTests
    parse
    (\pos ls ->
       Unexpected pos $
       Set.map (\case; Common.Eof -> Eof; Common.Char c -> Char c; Common.Name n -> Name n) ls
    )
    "infinity"
