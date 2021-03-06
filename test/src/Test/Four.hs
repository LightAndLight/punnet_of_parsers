{-# language LambdaCase #-}
module Test.Four (fourTests) where

import qualified Data.Set as Set
import Test.Hspec (Spec)

import Four (Label(..), ParseError(Unexpected), parse)
import Test.Common (commonTests)
import qualified Test.Common as Common

fourTests :: Spec
fourTests =
  commonTests
    parse
    (\pos ls ->
       Unexpected pos $
       Set.map (\case; Common.Eof -> Eof; Common.Char c -> Char c; Common.Name n -> Name n) ls
    )
    "four"
