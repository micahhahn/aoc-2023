module Parser
  ( module Parsing
  , module Parsing.Combinators
  , module Parsing.String
  , module Parsing.String.Basic
  , anyDigit
  , number
  ) where

import Prelude

import Parsing (Parser, position, runParser, Position(..))
import Parsing.Combinators (many, many1, choice, sepBy1, lookAhead)
import Data.Char (toCharCode)
import Data.Foldable (foldl)
import Data.List.Types (toList)
import Parsing.String (anyChar, satisfy, string, char)
import Parsing.String.Basic (oneOf)

anyDigit :: Parser String Char
anyDigit = satisfy (\c -> c >= '0' && c <= '9')

number :: Parser String Int
number =
  many1 anyDigit <#>
    ( \digits ->
        toList digits
          # foldl (\accum c -> (toCharCode c - toCharCode '0') + accum * 10) 0
    )