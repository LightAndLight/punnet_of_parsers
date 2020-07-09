{-# language DeriveGeneric #-}
{-# language GeneralizedNewtypeDeriving #-}
module One where

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
      Text ->
      Pos ->
      Set Label ->
      (Bool, Text, Pos, Set Label, Maybe a)
  }

parse :: Parser a -> Text -> Either ParseError a
parse (Parser p) input =
  let
    (_, _, Pos pos, ex, res) = p input 0 mempty
  in
    case res of
      Nothing -> Left $ Unexpected pos ex
      Just a -> Right a

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input pos ex ->
    let
      (consumed, input', pos', ex', a) = p input pos ex
    in
      (consumed, input', pos', ex', fmap f a)

instance Applicative Parser where
  pure a = Parser $ \input pos ex -> (False, input, pos, ex, Just a)
  Parser pf <*> Parser pa =
    Parser $ \input pos ex ->
      let
        (fConsumed, input', pos', ex', rf) = pf input pos ex
      in
        case rf of
          Nothing ->
            (fConsumed, input', pos', ex', Nothing)
          Just f ->
            let
              (aConsumed, input'', pos'', ex'', ra) = pa input' pos' ex'
            in
              case ra of
                Nothing ->
                  (fConsumed || aConsumed, input'', pos'', ex'', Nothing)
                Just a ->
                  (fConsumed || aConsumed, input'', pos'', ex'', Just (f a))

instance Alternative Parser where
  empty = Parser $ \input pos ex -> (False, input, pos, ex, Nothing)
  Parser pa <|> Parser pb =
    Parser $ \input pos ex ->
    let
      (aConsumed, input', pos', ex', ra) = pa input pos ex
    in
      case ra of
        Nothing ->
          if aConsumed
          then (aConsumed, input', pos', ex', ra)
          else pb input pos ex'
        Just{} ->
          (aConsumed, input', pos', ex', ra)

instance Parsing Parser where
  try (Parser p) =
    Parser $ \input pos ex ->
    let
      (_, input', pos', ex', res) = p input pos ex
    in
      (False, input', pos', ex', res)
  (<?>) (Parser p) n =
    Parser $ \input pos ex ->
    let
      (consumed, input', pos', _, res) = p input pos ex
    in
      (consumed, input', pos', Set.insert (Name n) ex, res)
  notFollowedBy (Parser p) =
    Parser $ \input pos ex ->
    let
      (_, _, _, _, res) = p input pos ex
    in
      case res of
        Nothing -> (False, input, pos, ex, Just ())
        Just{} -> (False, input, pos, ex, Nothing)
  unexpected _ = empty
  eof =
    Parser $ \input pos ex ->
    if Text.null input
    then (False, input, pos, ex, Just ())
    else (False, input, pos, Set.insert Eof ex, Nothing)

instance CharParsing Parser where
  satisfy f =
    Parser $ \input pos ex ->
    case Text.uncons input of
      Just (c, input') | f c ->
        (True, input', pos + 1, mempty, Just c)
      _ ->
        (False, input, pos, ex, Nothing)
  char c =
    Parser $ \input pos ex ->
    case Text.uncons input of
      Just (c', input') | c == c' ->
        (True, input', pos + 1, mempty, Just c)
      _ ->
        (False, input, pos, Set.insert (Char c) ex, Nothing)
