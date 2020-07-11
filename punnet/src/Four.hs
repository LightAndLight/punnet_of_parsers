{-# language DeriveGeneric #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MagicHash, UnboxedTuples, UnboxedSums, PatternSynonyms #-}
module Four where

import Control.Applicative (Alternative(..))
import Control.DeepSeq (NFData)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Exts (Int(I#), Int#, orI#, (+#))
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

type Pos# = Int#

type Maybe# a = (# (# #) | a #)

pattern Nothing# :: Maybe# a
pattern Nothing# = (# (# #) | #)

pattern Just# :: a -> Maybe# a
pattern Just# a = (# | a #)

{-# complete Nothing#, Just# #-}

fmap# :: (a -> b) -> Maybe# a -> Maybe# b
fmap# f m =
  case m of
    Nothing# -> Nothing#
    Just# a -> Just# (f a)

type Consumed# = Int#

newtype Parser a
  = Parser
  { unParser ::
      Text ->
      Pos# ->
      Set Label ->
      (# Consumed#, Text, Pos#, Set Label, Maybe# a #)
  }

parse :: Parser a -> Text -> Either ParseError a
parse (Parser p) input =
  case p input 0# mempty of
    (# _, _, pos, ex, res #) ->
      case res of
        Nothing# -> Left $ Unexpected (I# pos) ex
        Just# a -> Right a

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input pos ex ->
    case p input pos ex of
      (# consumed, input', pos', ex', a #) ->
        (# consumed, input', pos', ex', fmap# f a #)

instance Applicative Parser where
  pure a = Parser $ \input pos ex -> (# 0#, input, pos, ex, Just# a #)
  Parser pf <*> Parser pa =
    {-# SCC apply #-}
    Parser $ \input pos ex ->
      case pf input pos ex of
        (# fConsumed, input', pos', ex', rf #) ->
          case rf of
            Nothing# ->
              (# fConsumed, input', pos', ex', Nothing# #)
            Just# f ->
              case pa input' pos' ex' of
                (# aConsumed, input'', pos'', ex'', ra #) ->
                  case ra of
                    Nothing# ->
                      (# orI# fConsumed aConsumed, input'', pos'', ex'', Nothing# #)
                    Just# a ->
                      (# orI# fConsumed aConsumed, input'', pos'', ex'', Just# (f a) #)

instance Alternative Parser where
  empty = Parser $ \input pos ex -> (# 0#, input, pos, ex, Nothing# #)
  Parser pa <|> Parser pb =
    {-# SCC alt #-}
    Parser $ \input pos ex ->
    case pa input pos ex of
      (# aConsumed, input', pos', ex', ra #) ->
        case ra of
          Nothing# ->
            case aConsumed of
              1# -> (# aConsumed, input', pos', ex', ra #)
              _ -> pb input pos ex'
          Just#{} ->
            (# aConsumed, input', pos', ex', ra #)

instance Parsing Parser where
  try (Parser p) =
    Parser $ \input pos ex ->
    case p input pos ex of
      (# _, input', pos', ex', res #) ->
        (# 0#, input', pos', ex', res #)
  (<?>) (Parser p) n =
    Parser $ \input pos ex ->
    case p input pos ex of
      (# consumed, input', pos', _, res #) ->
        (# consumed, input', pos', Set.insert (Name n) ex, res #)
  notFollowedBy (Parser p) =
    Parser $ \input pos ex ->
    case p input pos ex of
      (# _, _, _, _, res #) ->
        case res of
          Nothing# -> (# 0#, input, pos, ex, Just# () #)
          Just#{} -> (# 0#, input, pos, ex, Nothing# #)
  unexpected _ = empty
  eof =
    Parser $ \input pos ex ->
    if Text.null input
    then (# 0#, input, pos, ex, Just# () #)
    else (# 0#, input, pos, Set.insert Eof ex, Nothing# #)

instance CharParsing Parser where
  satisfy f =
    {-# SCC satisfy #-}
    Parser $ \input pos ex ->
    case Text.uncons input of
      Just (c, input') | f c ->
        (# 1#, input', pos +# 1#, mempty, Just# c #)
      _ ->
        (# 0#, input, pos, ex, Nothing# #)
  char c =
    {-# SCC char #-}
    Parser $ \input pos ex ->
    case Text.uncons input of
      Just (c', input') | c == c' ->
        (# 1#, input', pos +# 1#, mempty, Just# c #)
      _ ->
        (# 0#, input, pos, Set.insert (Char c) ex, Nothing# #)
