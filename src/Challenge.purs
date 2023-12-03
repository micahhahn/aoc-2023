module Challenge where

import Data.Maybe (Maybe)

type Challenge =
  { examplePrompt :: Array String
  , exampleAnswer :: String
  , solver :: String -> String
  , solution :: Maybe String
  }