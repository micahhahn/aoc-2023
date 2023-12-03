module Day1
  ( challenge1
  ) where

import Prelude
import Challenge (Challenge)
import Data.Maybe (Maybe(..))

challenge1 :: Challenge
challenge1 =
  { examplePrompt:
      [ "1abc2"
      , "pqr3stu8vwx"
      , "a1b2c3d4e5f"
      , "treb7uchet"
      ]
  , exampleAnswer: "142"
  , solver: identity
  , solution: Nothing
  }