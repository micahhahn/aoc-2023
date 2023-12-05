module Day3
  ( challenge1
  , allNumberPos
  , neighbors
  ) where

import Prelude

import Challenge (Challenge)
import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.List (catMaybes, many, List, (:))
import Data.Foldable (sum)
import Data.List as List
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Parser (Parser, Position(..), anyChar, oneOf, position, number, runParser)

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
  symbol <- oneOf [ '*', '#', '+', '$', '/', '&', '=', '@', '%', '-' ]
  Position { line, column } <- position
  pure (((line - 1) `Tuple` (column - 2)) `Tuple` symbol)

symbolMap :: Parser String (Map (Tuple Int Int) Char)
symbolMap =
  many ((symbolParser <#> Just) <|> (anyChar *> pure Nothing))
    <#> catMaybes
    <#> Map.fromFoldable

type NumberPos =
  { line :: Int
  , startCol :: Int
  , endCol :: Int
  , num :: Int
  }

numberPos :: Parser String NumberPos
numberPos = do
  Position startPosition <- position
  num <- number
  Position endPosition <- position
  pure { line: startPosition.line - 1, startCol: startPosition.column - 1, endCol: endPosition.column - 2, num: num }

allNumberPos :: Parser String (List NumberPos)
allNumberPos = do
  many ((numberPos <#> Just) <|> (anyChar *> pure Nothing))
    <#> catMaybes

neighbors :: NumberPos -> List (Tuple Int Int)
neighbors { line, startCol, endCol } =
  Tuple line (startCol - 1)
    : Tuple line (endCol + 1)
    :
      ( List.range (startCol - 1) (endCol + 1)
          # List.concatMap (\col -> Tuple (line - 1) col : Tuple (line + 1) col : List.Nil)
      )

solution1 :: String -> String
solution1 input =
  case runParser input symbolMap /\ runParser input allNumberPos of
    Right symbols /\ Right numbers ->
      numbers
        # List.filter
            ( \num ->
                neighbors num
                  # List.any (\neighbor -> Map.member neighbor symbols)
            )
        # map (\x -> x.num)
        # sum
        # Int.toStringAs Int.decimal

    _ ->
      ""