{-# language LambdaCase #-}
module Test.Seven (sevenTests) where

import qualified Data.Set as Set
import Test.Hspec (Spec)

import Seven (Label(..), ParseError(Unexpected), parse)
import Test.Common (commonTests)
import qualified Test.Common as Common

sevenTests :: Spec
sevenTests =
  commonTests
    parse
    (\pos ls ->
       Unexpected pos $
       Set.map (\case; Common.Eof -> Eof; Common.Char c -> Char c; Common.Name n -> Name n) ls
    )
    "seven"
