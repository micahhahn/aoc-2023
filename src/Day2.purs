module Day2
  ( challenge1
  , x
  ) where

import Prelude

import Challenge (Challenge)
import Control.Alt ((<|>))
import Data.Char (toCharCode)
import Data.Either (Either(..))
import Data.Foldable (foldl, sum)
import Data.Filterable (filterMap)
import Data.Int as Int
import Data.List (List)
import Data.Array (filter)
import Data.List (all)
import Data.List.Types (toList)
import Data.Maybe (Maybe(..))
import Data.String as String
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.CodePoints (string, anyDigit, char)
import Text.Parsing.StringParser.Combinators (many1, sepBy1)

challenge1 :: Challenge
challenge1 =
  { examplePrompt:
      [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
      , "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
      , "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
      , "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
      , "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
      ]
  , exampleAnswer: "8"
  , solver: solution1
  , promptPath: "assets/day2.txt"
  , solution: Nothing
  }

type Set =
  { red :: Int
  , blue :: Int
  , green :: Int
  }

type Game = { gameId :: Int, sets :: List Set }

number :: Parser Int
number = many1 anyDigit <#>
  ( \digits ->
      toList digits
        # foldl (\accum c -> (toCharCode c - toCharCode '0') + accum * 10) 0
  )

data Color
  = Red
  | Green
  | Blue

color :: Parser Color
color =
  (string "red" *> pure Red)
    <|> (string "green" *> pure Green)
    <|> (string "blue" *> pure Blue)

draw :: Parser { amount :: Int, color :: Color }
draw = do
  amount <- number
  _ <- char ' '
  color_ <- color
  pure { amount: amount, color: color_ }

set :: Parser Set
set = do
  sepBy1 draw (string ", ")
    <#> foldl
      ( \r d ->
          case d.color of
            Red ->
              r { red = d.amount }
            Green ->
              r { green = d.amount }
            Blue ->
              r { blue = d.amount }
      )
      { red: 0, green: 0, blue: 0 }

game :: Parser Game
game = do
  _ <- string "Game "
  gameId <- number
  _ <- string ": "
  sets <- sepBy1 set (string "; ") <#> toList
  pure { gameId: gameId, sets: sets }

gameCheck :: Game -> Boolean
gameCheck { sets } =
  all (\set -> set.red <= 12 && set.green <= 13 && set.blue <= 14) sets

x = map (runParser game) challenge1.examplePrompt

solution1 :: String -> String
solution1 input =
  let
    lines = String.split (String.Pattern "\n") input

    possibleGames =
      filterMap
        ( \line -> case runParser game line of
            Left _ ->
              Nothing
            Right game ->
              if gameCheck game then
                Just game
              else
                Nothing
        )
        lines
  in
    map (\game -> game.gameId) possibleGames
      # sum
      # Int.toStringAs Int.decimal