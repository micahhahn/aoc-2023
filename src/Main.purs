module Main
  ( main
  ) where

import Prelude

import Challenge (Challenge)
import Data.Traversable (traverse_)
import Day1 as Day1
import Day2 as Day2
import Day3 as Day3
import Day4 as Day4
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

challenges :: Array Challenge
challenges =
  [ Day1.challenge1
  , Day1.challenge2
  , Day2.challenge1
  , Day2.challenge2
  , Day3.challenge1
  , Day3.challenge2
  , Day4.challenge1
  , Day4.challenge2
  ]

main :: Effect (Unit)
main =
  traverse_
    ( \challenge -> do
        input <- readTextFile UTF8 challenge.promptPath
        log $ challenge.solver input
    )
    challenges