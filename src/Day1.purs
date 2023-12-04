module Day1
  ( challenge1
  , challenge2
  ) where

import Prelude

import Challenge (Challenge)
import Control.Alt ((<|>))
import Data.Char (toCharCode)
import Data.Either (Either(..))
import Data.Foldable (foldl, sum)
import Data.Int as Int
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple.Nested ((/\))
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.CodePoints (anyChar, satisfy, string)
import Text.Parsing.StringParser.Combinators (choice, lookAhead, many)

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
  , solution: Just "54916"
  }

parseLine :: String -> Int
parseLine line =
  let
    maybeResult =
      foldl
        ( \maybeAccum char ->
            if char >= '0' && char <= '9' then
              case maybeAccum of
                Nothing ->
                  Just { first: char, last: char }
                Just accum ->
                  Just (accum { last = char })
            else maybeAccum
        )
        Nothing
        (toCharArray line)
  in
    case maybeResult of
      Nothing ->
        0

      Just { first, last } ->
        case Int.fromString (fromCharArray [ first, last ]) of
          Nothing ->
            0

          Just x ->
            x

solution1 :: String -> String
solution1 input =
  let
    lines = String.split (String.Pattern "\n") input
  in
    sum (map parseLine lines)
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
  , solution: Just "54728"
  }

solution2 :: String -> String
solution2 input =
  let
    lines = String.split (String.Pattern "\n") input
  in
    sum (map parseLine2 lines)
      # Int.toStringAs Int.decimal

numbers :: Array (Parser Int)
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

digit :: Parser Int
digit =
  satisfy (\c -> c >= '0' && c <= '9')
    <#> (\c -> toCharCode c - toCharCode '0')

parser2 :: Parser (List (Maybe Int))
parser2 =
  many ((choice numbers <#> Just) <|> (anyChar *> pure Nothing))

parseLine2 :: String -> Int
parseLine2 line =
  case runParser parser2 line of
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