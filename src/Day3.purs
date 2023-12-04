module Day3
  ( challenge1
  , symbolMapParser
  ) where

import Prelude

import Challenge (Challenge)
import Control.Alt ((<|>))
import Data.List (catMaybes, many)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser (Parser, position)
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser.String (oneOf, anyChar)

challenge1 :: Challenge
challenge1 =
  { examplePrompt:
      [ "467..114.."
      , "...*......"
      , "..35..633."
      , "......#..."
      , "617*......"
      , ".....+.58."
      , "..592....."
      , "......755."
      , "...$.*...."
      , ".664.598.."
      ]
  , exampleAnswer: "4361"
  , solver: solution1
  , promptPath: "assets/day3.txt"
  , solution: Nothing
  }

symbolParser :: Parser String (Tuple (Tuple Int Int) Char)
symbolParser = do
  symbol <- oneOf [ '*', '#', '+', '$', '/', '&', '=', '@', '%' ]
  Position { line, column } <- position
  pure (((line - 1) `Tuple` (column - 1)) `Tuple` symbol)

symbolMapParser :: Parser String (Map (Tuple Int Int) Char)
symbolMapParser = do
  many ((symbolParser <#> Just) <|> (anyChar *> pure Nothing))
    <#> catMaybes
    <#> Map.fromFoldable

solution1 :: String -> String
solution1 = identity