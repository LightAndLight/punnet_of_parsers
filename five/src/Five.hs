{-# language DeriveGeneric #-}
{-# language BangPatterns, UnboxedTuples, UnboxedSums #-}
{-# language MagicHash #-}
module Five where

import Control.Applicative (Alternative(..))
import Control.DeepSeq (NFData)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import GHC.Exts (Int#, Int(I#), (+#), orI#)
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

type Consumed# = Int#
type Pos# = Int#

newtype Parser a
  = Parser
  { unParser ::
      (# Text, Pos#, Set Label #) ->
      (# Consumed#, Text, Pos#, Set Label, (# (# #) | a #) #)
  }

parse :: Parser a -> Text -> Either ParseError a
parse (Parser p) input =
  let
    !(# _, _, pos, ex, res #) = p (# input, 0#, mempty #)
  in
    case res of
      (# (# #) | #) -> Left $ Unexpected (I# pos) ex
      (# | a #) -> Right a

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \(# input, pos, ex #) ->
    let
      !(# consumed, input', pos', ex', ra #) = p (# input, pos, ex #)
    in
      (# consumed
      , input'
      , pos'
      , ex'
      , case ra of
          (# (# #) | #) -> (# (# #) | #)
          (# | a #) -> (# | f a #)
      #)

instance Applicative Parser where
  pure a = Parser $ \(# input, pos, ex #) -> (# 0#, input, pos, ex, (# | a #) #)
  Parser pf <*> Parser pa =
    Parser $ \(# input, pos, ex #) ->
      let
        !(# fConsumed, input', pos', ex', rf #) = pf (# input, pos, ex #)
      in
        case rf of
          (# (# #) | #) ->
            (# fConsumed, input', pos', ex', (# (# #) | #) #)
          (# | f #) ->
            let
              !(# aConsumed, input'', pos'', ex'', ra #) = pa (# input', pos', ex' #)
            in
              (# orI# fConsumed aConsumed
              , input''
              , pos''
              , ex''
              , case ra of
                  (# (# #) | #) ->
                    (# (# #) | #)
                  (# | a #) ->
                    (# | f a #)
              #)

instance Alternative Parser where
  empty = Parser $ \(# input, pos, ex #) -> (# 0#, input, pos, ex, (# (# #) | #) #)
  Parser pa <|> Parser pb =
    Parser $ \(# input, pos, ex #) ->
    let
      !(# aConsumed, input', pos', ex', ra #) = pa (# input, pos, ex #)
    in
      case ra of
        (# (# #) | #) ->
          case aConsumed of
            1# -> (# aConsumed, input', pos', ex', ra #)
            _ -> pb (# input, pos, ex' #)
        (# | _ #) ->
          (# aConsumed, input', pos', ex', ra #)

instance Parsing Parser where
  try (Parser p) =
    Parser $ \(# input, pos, ex #) ->
    let
      !(# _, input', pos', ex', res #) = p (# input, pos, ex #)
    in
      (# 0#, input', pos', ex', res #)
  (<?>) (Parser p) n =
    Parser $ \(# input, pos, ex #) ->
    let
      !(# consumed, input', pos', _, res #) = p (# input, pos, ex #)
    in
      (# consumed, input', pos', Set.insert (Name n) ex, res #)
  notFollowedBy (Parser p) =
    Parser $ \(# input, pos, ex #) ->
    let
      !(# _, _, _, _, res #) = p (# input, pos, ex #)
    in
      case res of
        (# (# #) | #) -> (# 0#, input, pos, ex, (# | () #) #)
        (# | _ #) -> (# 0#, input, pos, ex, (# (# #) | #) #)
  unexpected _ = empty
  eof =
    Parser $ \(# input, pos, ex #) ->
    if Text.null input
    then (# 0#, input, pos, ex, (# | () #) #)
    else (# 0#, input, pos, Set.insert Eof ex, (# (# #) | #) #)

instance CharParsing Parser where
  satisfy f =
    Parser $ \(# input, pos, ex #) ->
    case Text.uncons input of
      Just (c, input') | f c ->
        (# 1#, input', pos +# 1#, mempty, (# | c #) #)
      _ ->
        (# 0#, input, pos, ex, (# (# #) | #) #)
  char c =
    Parser $ \(# input, pos, ex #) ->
    case Text.uncons input of
      Just (c', input') | c == c' ->
        (# 1#, input', pos +# 1#, mempty, (# | c #) #)
      _ ->
        (# 0#, input, pos, Set.insert (Char c) ex, (# (# #) | #) #)
