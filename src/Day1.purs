module Day1
  ( challenge1
  , challenge2
  ) where

import Prelude

import Challenge (Challenge)
import Control.Alt ((<|>))
import Data.Char (toCharCode)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Int as Int
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple.Nested ((/\))
import Parser (Parser, runParser, anyChar, satisfy, string, choice, lookAhead, many)

challenge1 :: Challenge
challenge1 =
  { examplePrompt:
      [ "1abc2"
      , "pqr3stu8vwx"
      , "a1b2c3d4e5f"
      , "treb7uchet"
      ]
  , exampleAnswer: "142"
  , solver: solution1
  , promptPath: "assets/day1.txt"
  , solution: Just "54916"
  }

digit :: Parser String Int
digit =
  satisfy (\c -> c >= '0' && c <= '9')
    <#> (\c -> toCharCode c - toCharCode '0')

parser1 :: Parser String (List (Maybe Int))
parser1 =
  many ((digit <#> Just) <|> (anyChar *> pure Nothing))

parseLine :: Parser String (List (Maybe Int)) -> String -> Int
parseLine parser line =
  case runParser line parser of
    Left _ ->
      0
    Right maybeValues ->
      let
        values = List.catMaybes maybeValues
      in
        case List.head values /\ List.last values of
          Just first /\ Just last ->
            first * 10 + last

          _ ->
            0

solution1 :: String -> String
solution1 input =
  let
    lines = String.split (String.Pattern "\n") input
  in
    sum (map (parseLine parser1) lines)
      # Int.toStringAs Int.decimal

challenge2 :: Challenge
challenge2 =
  { examplePrompt:
      [ "two1nine"
      , "eightwothree"
      , "abcone2threexyz"
      , "xtwone3four"
      , "4nineeightseven2"
      , "zoneight234"
      , "7pqrstsixteen"
      ]
  , exampleAnswer: "281"
  , solver: solution2
  , promptPath: "assets/day1.txt"
  , solution: Just "54728"
  }

numbers :: Array (Parser String Int)
numbers =
  [ lookAhead (string "one") *> anyChar *> pure 1
  , lookAhead (string "two") *> anyChar *> pure 2
  , lookAhead (string "three") *> anyChar *> pure 3
  , lookAhead (string "four") *> anyChar *> pure 4
  , lookAhead (string "five") *> anyChar *> pure 5
  , lookAhead (string "six") *> anyChar *> pure 6
  , lookAhead (string "seven") *> anyChar *> pure 7
  , lookAhead (string "eight") *> anyChar *> pure 8
  , lookAhead (string "nine") *> anyChar *> pure 9
  , digit
  ]

parser2 :: Parser String (List (Maybe Int))
parser2 =
  many ((choice numbers <#> Just) <|> (anyChar *> pure Nothing))

solution2 :: String -> String
solution2 input =
  let
    lines = String.split (String.Pattern "\n") input
  in
    sum (map (parseLine parser2) lines)
      # Int.toStringAs Int.decimal