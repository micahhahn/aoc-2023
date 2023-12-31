module Parser
  ( module Parsing
  , module Parsing.Combinators
  , module Parsing.String
  , module Parsing.String.Basic
  , anyDigit
  , number
  , bigint
  , space
  , newline
  ) where

import Prelude

import Data.Char (toCharCode)
import Data.Foldable (foldl)
import Data.List.Types (toList)
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Parsing (Parser, position, runParser, Position(..))
import Parsing.Combinators (many, many1, choice, sepBy, sepBy1, lookAhead, try, sepEndBy)
import Parsing.String (anyChar, satisfy, string, char)
import Parsing.String.Basic (oneOf, whiteSpace)

anyDigit :: Parser String Char
anyDigit = satisfy (\c -> c >= '0' && c <= '9')

number :: Parser String Int
number =
  many1 anyDigit <#>
    ( \digits ->
        toList digits
          # foldl (\accum c -> (toCharCode c - toCharCode '0') + accum * 10) 0
    )

bigint :: Parser String BigInt
bigint =
  many1 anyDigit <#>
    ( \digits ->
        toList digits
          # foldl (\accum c -> BigInt.fromInt (toCharCode c - toCharCode '0') + accum * (BigInt.fromInt 10)) (BigInt.fromInt 0)
    )

space :: Parser String Char
space = char ' '

newline :: Parser String Char
newline = char '\n'