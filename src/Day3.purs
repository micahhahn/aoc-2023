module Day3
  ( challenge1
  , challenge2
  ) where

import Prelude

import Challenge (Challenge)
import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Foldable (sum, foldl)
import Data.Int as Int
import Data.List (catMaybes, many, List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Parser (Parser, Position(..), anyChar, oneOf, position, number, runParser)

examplePrompt :: Array String
examplePrompt =
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

challenge1 :: Challenge
challenge1 =
  { examplePrompt: examplePrompt
  , exampleAnswer: "4361"
  , solver: solution1
  , promptPath: "assets/day3.txt"
  , solution: Just "538046"
  }

type SymbolPos =
  { line :: Int
  , col :: Int
  , symbol :: Char
  }

symbolPos :: Parser String SymbolPos
symbolPos = do
  symbol <- oneOf [ '*', '#', '+', '$', '/', '&', '=', '@', '%', '-' ]
  Position { line, column } <- position
  pure { line: (line - 1), col: (column - 2), symbol: symbol }

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

data Object
  = Symbol SymbolPos
  | PartNumber NumberPos

grid :: Parser String (List Object)
grid =
  many
    ( (symbolPos <#> Symbol >>> Just)
        <|> (numberPos <#> PartNumber >>> Just)
        <|> (anyChar *> pure Nothing)
    )
    <#> catMaybes

parseGrid :: String -> { symbols :: List SymbolPos, parts :: List NumberPos }
parseGrid input =
  case runParser input grid of
    Right objects ->
      foldl
        ( \accum object ->
            case object of
              Symbol s ->
                accum { symbols = s : accum.symbols }
              PartNumber n ->
                accum { parts = n : accum.parts }
        )
        { symbols: Nil, parts: Nil }
        objects
    Left _ ->
      { symbols: Nil, parts: Nil }

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
  let
    { symbols, parts } = parseGrid input
    symbolMap =
      symbols
        # map (\s -> Tuple (Tuple s.line s.col) s.symbol)
        # Map.fromFoldable
  in
    parts
      # List.filter
          ( \num ->
              neighbors num
                # List.any (\neighbor -> Map.member neighbor symbolMap)
          )
      # map (\x -> x.num)
      # sum
      # Int.toStringAs Int.decimal

challenge2 :: Challenge
challenge2 =
  { examplePrompt: examplePrompt
  , exampleAnswer: "467835"
  , solver: solution2
  , promptPath: "assets/day3.txt"
  , solution: Nothing
  }

solution2 :: String -> String
solution2 input =
  let
    { symbols, parts } = parseGrid input
  in
    ""