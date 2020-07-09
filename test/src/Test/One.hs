{-# language LambdaCase #-}
module Test.One (oneTests) where

import qualified Data.Set as Set
import Test.Hspec (Spec)

import One (Label(..), ParseError(Unexpected), parse)
import Test.Common (commonTests)
import qualified Test.Common as Common

oneTests :: Spec
oneTests =
  commonTests
    parse
    (\pos ls ->
       Unexpected pos $
       Set.map (\case; Common.Eof -> Eof; Common.Char c -> Char c; Common.Name n -> Name n) ls
    )
    "one"
