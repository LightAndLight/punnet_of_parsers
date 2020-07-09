{-# language BangPatterns #-}
{-# language DeriveGeneric #-}
{-# language UnboxedTuples, UnboxedSums #-}
{-# language MagicHash #-}
module Five where

import Control.Applicative (Alternative(..))
import Control.DeepSeq (NFData)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import GHC.Exts (Int#, orI#)
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
type Pos = Int

newtype Parser a
  = Parser
  { unParser ::
      (# Text, Pos, Set Label #) ->
      (# Consumed#, Text, Pos, Set Label, (# (# #) | a #) #)
  }

parse :: Parser a -> Text -> Either ParseError a
parse (Parser p) input =
  case p (# input, 0, mempty #) of
    (# _, _, pos, ex, res #) ->
      case res of
        (# (# #) | #) -> Left $ Unexpected pos ex
        (# | a #) -> Right a

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input ->
    case p input of
      (# consumed, input', pos', ex', ra #) ->
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
    Parser $ \input ->
    case pf input of
      (# fConsumed, input', pos', ex', rf #) ->
        case rf of
          (# (# #) | #) ->
            (# fConsumed, input', pos', ex', (# (# #) | #) #)
          (# | f #) ->
            case pa (# input', pos', ex' #) of
              (# aConsumed, input'', pos'', ex'', ra #) ->
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
    case pa (# input, pos, ex #) of
      (# aConsumed, input', pos', ex', ra #) ->
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
    case p (# input, pos, ex #) of
      (# _, input', pos', ex', res #) ->
        (# 0#, input', pos', ex', res #)
  (<?>) (Parser p) n =
    Parser $ \(# input, pos, ex #) ->
    case p (# input, pos, ex #) of
      (# consumed, input', pos', _, res #) ->
        (# consumed, input', pos', Set.insert (Name n) ex, res #)
  notFollowedBy (Parser p) =
    Parser $ \(# input, pos, ex #) ->
    case p (# input, pos, ex #) of
      (# _, _, _, _, res #) ->
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
        let !pos' = pos + 1 in
        (# 1#, input', pos', mempty, (# | c #) #)
      _ ->
        (# 0#, input, pos, ex, (# (# #) | #) #)
  char c =
    Parser $ \(# input, pos, ex #) ->
    case Text.uncons input of
      Just (c', input') | c == c' ->
        let !pos' = pos + 1 in
        (# 1#, input', pos', mempty, (# | c #) #)
      _ ->
        (# 0#, input, pos, Set.insert (Char c) ex, (# (# #) | #) #)
