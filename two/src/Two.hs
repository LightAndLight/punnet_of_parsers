{-# language DeriveGeneric #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language RankNTypes #-}
module Two where

import Control.Applicative (Alternative(..))
import Control.DeepSeq (NFData)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Text.Parser.Combinators (Parsing(..))
import Text.Parser.Char (CharParsing(..))

data Label
  = Eof
  | Char Char
  | Name String
  deriving (Eq, Ord, Show, Generic)
instance NFData Label

data ParseError
  = Unexpected
  { position :: Int
  , expected :: Set Label
  } deriving (Eq, Show, Generic)
instance NFData ParseError

newtype Pos = Pos { unPos :: Int }
  deriving Num

newtype Parser a
  = Parser
  { unParser ::
      forall r.
      Text ->
      Pos ->
      Set Label ->
      (Text -> Pos -> Set Label -> r) -> -- uncommitted failure
      (a -> Text -> Pos -> Set Label -> r) -> -- uncommitted success
      (Text -> Pos -> Set Label -> r) -> -- committed failure
      (a -> Text -> Pos -> Set Label -> r) -> -- committed success
      r
  }

parse :: Parser a -> Text -> Either ParseError a
parse (Parser p) input =
  p input 0 mempty failure success failure success
  where
    failure = \_ (Pos pos) ex -> Left $ Unexpected pos ex
    success = \a _ _ _ -> Right a

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input pos ex ucFail ucSuccess cFail cSuccess ->
    p input pos ex
      ucFail
      (ucSuccess . f)
      cFail
      (cSuccess . f)

instance Applicative Parser where
  pure a = Parser $ \input pos ex _ ucSuccess _ _ -> ucSuccess a input pos ex
  Parser pf <*> Parser pa =
    Parser $ \input pos ex ucFail ucSuccess cFail cSuccess ->
    pf input pos ex
      ucFail
      (\f input' pos' ex' ->
         pa input' pos' ex'
           ucFail
           (ucSuccess . f)
           cFail
           (cSuccess . f)
      )
      cFail
      (\f input' pos' ex' ->
         pa input' pos' ex'
           cFail
           (cSuccess  . f)
           cFail
           (cSuccess  . f)
      )

instance Alternative Parser where
  empty = Parser $ \input pos ex ucFail _ _ _ -> ucFail input pos ex
  Parser pa <|> Parser pb =
    Parser $ \input pos ex ucFail ucSuccess cFail cSuccess ->
    pa input pos ex
      (\input' pos' ex' ->
         pb input' pos' ex'
           ucFail
           ucSuccess
           cFail
           cSuccess
      )
      ucSuccess
      cFail
      cSuccess

instance Parsing Parser where
  try (Parser p) =
    Parser $ \input pos ex ucFail ucSuccess _ _ ->
    p input pos ex
      (\_ _ _ -> ucFail input pos ex)
      (\a _ _ _ -> ucSuccess a input pos ex)
      (\_ _ _ -> ucFail input pos ex)
      (\a _ _ _ -> ucSuccess a input pos ex)
  (<?>) (Parser p) n =
    Parser $ \input pos ex ucFail ucSuccess cFail cSuccess ->
    let
      ex' = Set.insert (Name n) ex
    in
      p input pos ex
        (\input' pos' _ -> ucFail input' pos' ex')
        (\a input' pos' _ -> ucSuccess a input' pos' ex')
        (\input' pos' _ -> cFail input' pos' ex')
        (\a input' pos' _ -> cSuccess a input' pos' ex')
  notFollowedBy (Parser p) =
    Parser $ \input pos ex ucFail ucSuccess _ _ ->
    p input pos ex
      (\_ _ _ -> ucSuccess () input pos ex)
      (\_ _ _ _ -> ucFail input pos ex)
      (\_ _ _ -> ucSuccess () input pos ex)
      (\_ _ _ _ -> ucFail input pos ex)
  unexpected _ = empty
  eof =
    Parser $ \input pos ex ucFail ucSuccess _ _ ->
    if Text.null input
    then ucSuccess () input pos ex
    else ucFail input pos (Set.insert Eof ex)

instance CharParsing Parser where
  satisfy f =
    Parser $ \input pos ex ucFail _ _ cSuccess ->
    case Text.uncons input of
      Just (c, input') | f c ->
        cSuccess c input' (pos + 1) mempty
      _ ->
        ucFail input pos ex
  char c =
    Parser $ \input pos ex ucFail _ _ cSuccess ->
    case Text.uncons input of
      Just (c', input') | c == c' ->
        cSuccess c input' (pos + 1) mempty
      _ ->
        ucFail input pos (Set.insert (Char c) ex)
