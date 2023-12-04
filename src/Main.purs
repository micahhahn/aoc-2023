module Main
  ( main
  ) where

import Prelude

import Challenge (Challenge)
import Data.Traversable (traverse_)
import Day1 as Day1
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

challenges :: Array { challenge :: Challenge, path :: String }
challenges =
  [ { challenge: Day1.challenge1, path: "assets/day1.txt" }
  , { challenge: Day1.challenge2, path: "assets/day1.txt" }
  ]

main :: Effect (Unit)
main =
  traverse_
    ( \x -> do
        input <- readTextFile UTF8 x.path
        log (x.challenge.solver input)
    )
    challenges