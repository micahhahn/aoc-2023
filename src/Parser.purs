module Parser
  ( module Text.Parsing.Parser
  , module Text.Parsing.Parser.Pos
  , module Text.Parsing.Parser.String
  , module Text.Parsing.Parser.Combinators
  , anyDigit
  , number
  ) where

import Prelude

import Text.Parsing.Parser (Parser, position, runParser)
import Text.Parsing.Parser.Combinators (many1)
import Data.Char (toCharCode)
import Text.Parsing.Parser.Pos (Position(..))
import Data.Foldable (foldl)
import Data.List.Types (toList)
import Text.Parsing.Parser.String (anyChar, oneOf, satisfy)

anyDigit :: Parser String Char
anyDigit = satisfy (\c -> c >= '0' && c <= '9')

number :: Parser String Int
number =
  many1 anyDigit <#>
    ( \digits ->
        toList digits
          # foldl (\accum c -> (toCharCode c - toCharCode '0') + accum * 10) 0
    )