module Day1
  ( challenge1
  , parseLine
  ) where

import Prelude

import Challenge (Challenge)
import Data.Foldable (foldl, sum)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Int as Int
import Data.String.CodeUnits (fromCharArray, toCharArray)

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
  , solution: Nothing
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