module Challenge where

import Data.Maybe (Maybe)

type Challenge =
  { name :: String
  , examplePrompt :: Array String
  , exampleAnswer :: String
  , solver :: String -> String
  , promptPath :: String
  , solution :: Maybe String
  }