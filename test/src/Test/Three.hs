{-# language LambdaCase #-}
module Test.Three (threeTests) where

import qualified Data.Set as Set
import Test.Hspec (Spec)

import Three (Label(..), ParseError(Unexpected), parse)
import Test.Common (commonTests)
import qualified Test.Common as Common

threeTests :: Spec
threeTests =
  commonTests
    parse
    (\pos ls ->
       Unexpected pos $
       Set.map (\case; Common.Eof -> Eof; Common.Char c -> Char c; Common.Name n -> Name n) ls
    )
    "three"
