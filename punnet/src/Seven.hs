{-# language DeriveGeneric #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MagicHash, UnboxedTuples, UnboxedSums, PatternSynonyms #-}
module Seven where

import Control.Applicative (Alternative(..))
import Control.DeepSeq (NFData)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Array
import qualified Data.Text.Internal
import GHC.Exts (Int(I#), Int#, orI#, (+#), ByteArray#)
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

type Text# = (# ByteArray#, Int#, Int# #)

fromText# :: Text# -> Text
fromText# (# arr, off, len #) =
  Data.Text.Internal.Text (Data.Text.Array.Array arr) (I# off) (I# len)

toText# :: Text -> Text#
toText# (Data.Text.Internal.Text (Data.Text.Array.Array arr) (I# off) (I# len)) =
  (# arr, off, len #)

newtype Parser a
  = Parser
  { unParser ::
      Text# ->
      Set Label ->
      (# Consumed#, Text#, Pos#, Set Label, Maybe# a #)
  }

parse :: Parser a -> Text -> Either ParseError a
parse (Parser p) input =
  case p (toText# input) mempty of
    (# _, _, pos, ex, res #) ->
      case res of
        Nothing# -> Left $ Unexpected (I# pos) ex
        Just# a -> Right a

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input ex ->
    case p input ex of
      (# consumed, input', pos', ex', a #) ->
        (# consumed, input', pos', ex', fmap# f a #)

instance Applicative Parser where
  pure a = Parser $ \input ex -> (# 0#, input, 0#, ex, Just# a #)
  Parser pf <*> Parser pa =
    {-# SCC apply #-}
    Parser $ \input ex ->
      case pf input ex of
        (# fConsumed, input', pos', ex', rf #) ->
          case rf of
            Nothing# ->
              (# fConsumed, input', pos', ex', Nothing# #)
            Just# f ->
              case pa input' ex' of
                (# aConsumed, input'', pos'', ex'', ra #) ->
                  case ra of
                    Nothing# ->
                      (# orI# fConsumed aConsumed, input'', pos' +# pos'', ex'', Nothing# #)
                    Just# a ->
                      (# orI# fConsumed aConsumed, input'', pos' +# pos'', ex'', Just# (f a) #)

instance Alternative Parser where
  empty = Parser $ \input ex -> (# 0#, input, 0#, ex, Nothing# #)
  Parser pa <|> Parser pb =
    {-# SCC alt #-}
    Parser $ \input ex ->
    case pa input ex of
      (# aConsumed, input', pos', ex', ra #) ->
        case ra of
          Nothing# ->
            case aConsumed of
              1# -> (# aConsumed, input', pos', ex', ra #)
              _ -> pb input ex'
          Just#{} ->
            (# aConsumed, input', pos', ex', ra #)

  {-# inline some #-}
  some (Parser pa) =
    Parser $ \input ex ->
    case pa input ex of
      (# consumed, input', pos', ex', ra #) ->
        case ra of
          Nothing# -> (# consumed, input', pos', ex', Nothing# #)
          Just# a ->
            go consumed (a :) input' pos' ex'
    where
      go consumed acc input pos ex =
        case pa input ex of
          (# consumed', input', pos', ex', ra #) ->
            let
              pos'' = pos +# pos'
            in
              case ra of
                Nothing# ->
                  case consumed' of
                    1# -> (# 1#, input', pos'', ex', Nothing# #)
                    _ -> (# consumed, input', pos'', ex', Just# (acc []) #)
                Just# a ->
                  go (orI# consumed consumed') (acc . (a :)) input' pos'' ex'

  many (Parser pa) =
    Parser (go 0# id 0#)
    where
      go consumed acc pos input ex =
        case pa input ex of
          (# consumed', input', pos', ex', ra #) ->
            let
              pos'' = pos +# pos'
            in
              case ra of
                Nothing# ->
                  case consumed' of
                    1# -> (# 1#, input', pos'', ex', Nothing# #)
                    _ -> (# consumed, input', pos'', ex', Just# (acc []) #)
                Just# a ->
                  go (orI# consumed consumed') (acc . (a :)) pos'' input' ex'

instance Parsing Parser where
  try (Parser p) =
    Parser $ \input ex ->
    case p input ex of
      (# _, input', pos', ex', res #) ->
        (# 0#, input', pos', ex', res #)
  (<?>) (Parser p) n =
    Parser $ \input ex ->
    case p input ex of
      (# consumed, input', pos', _, res #) ->
        (# consumed, input', pos', Set.insert (Name n) ex, res #)
  notFollowedBy (Parser p) =
    Parser $ \input ex ->
    case p input ex of
      (# _, _, _, _, res #) ->
        case res of
          Nothing# -> (# 0#, input, 0#, ex, Just# () #)
          Just#{} -> (# 0#, input, 0#, ex, Nothing# #)
  unexpected _ = empty
  eof =
    Parser $ \input ex ->
    if Text.null (fromText# input)
    then (# 0#, input, 0#, ex, Just# () #)
    else (# 0#, input, 0#, Set.insert Eof ex, Nothing# #)

instance CharParsing Parser where
  satisfy f =
    {-# SCC satisfy #-}
    Parser $ \input ex ->
    case Text.uncons (fromText# input) of
      Just (c, input') | f c ->
        (# 1#, toText# input', 1#, mempty, Just# c #)
      _ ->
        (# 0#, input, 0#, ex, Nothing# #)
  char c =
    {-# SCC char #-}
    Parser $ \input ex ->
    case Text.uncons (fromText# input) of
      Just (c', input') | c == c' ->
        (# 1#, toText# input', 1#, mempty, Just# c #)
      _ ->
        (# 0#, input, 0#, Set.insert (Char c) ex, Nothing# #)
