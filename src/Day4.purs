module Day4
  ( challenge1
  , challenge2
  ) where

import Prelude

import Challenge (Challenge)
import Data.Either (Either(..))
import Data.Foldable (foldl, sum)
import Data.Int as Int
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Parser (Parser, char, number, runParser, sepBy, string, whiteSpace)

examplePrompt :: Array String
examplePrompt =
  [ "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
  , "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
  , "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
  , "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
  , "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
  , "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
  ]

challenge1 :: Challenge
challenge1 =
  { examplePrompt: examplePrompt
  , exampleAnswer: "13"
  , solver: solution1
  , promptPath: "assets/day4.txt"
  , solution: Just "25571"
  }

type Card =
  { winningNumbers :: Set Int
  , elfNumbers :: List Int
  }

card :: Parser String Card
card = do
  _ <- string "Card "
  _ <- whiteSpace *> number
  _ <- string ": "
  winning <- (whiteSpace *> number) `sepBy` (char ' ')
  _ <- string " | "
  elf <- (whiteSpace *> number) `sepBy` (char ' ')
  pure { winningNumbers: Set.fromFoldable winning, elfNumbers: elf }

parseInput :: String -> List Card
parseInput input =
  case runParser input (card `sepBy` (char '\n')) of
    Left _ ->
      Nil
    Right x ->
      x

cardWinners :: Card -> Int
cardWinners { elfNumbers, winningNumbers } =
  elfNumbers
    # List.filter (\number -> Set.member number winningNumbers)
    # List.length

scoreCard :: Card -> Int
scoreCard c =
  cardWinners c
    # \n -> Int.pow 2 (n - 1)

solution1 :: String -> String
solution1 input =
  parseInput input
    # map scoreCard
    # sum
    # Int.toStringAs Int.decimal

challenge2 :: Challenge
challenge2 =
  { examplePrompt: examplePrompt
  , exampleAnswer: "30"
  , solver: solution2
  , promptPath: "assets/day4.txt"
  , solution: Nothing
  }

addCopies :: Int -> Int -> List Int -> List Int
addCopies _ 0 copies = copies
addCopies _ _ Nil = Nil
addCopies x n (copy : others) = (copy + x) : addCopies x (n - 1) others

solution2 :: String -> String
solution2 input =
  let
    cards = parseInput input
  in
    foldl
      ( \state c ->
          case state.copies of
            copy : otherCopies ->
              { sum: copy + state.sum
              , copies: addCopies copy (cardWinners c) otherCopies
              }

            _ ->
              -- Impossible
              state
      )
      { copies: map (const 1) cards, sum: 0 }
      cards
      # \x -> x.sum
          # Int.toStringAs Int.decimal
