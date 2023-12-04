module Test.Main where

import Prelude

import Challenge (Challenge)
import Data.String (joinWith)
import Data.Traversable (traverse_)
import Day1 as Day1
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Data.Maybe (Maybe(..))
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

challenges :: Array Challenge
challenges =
  [ Day1.challenge1
  , Day1.challenge2
  ]

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Challenge Examples Tests" do
    it "Should pass examples" do
      traverse_
        ( \challenge ->
            challenge.solver (joinWith "\n" challenge.examplePrompt) `shouldEqual` challenge.exampleAnswer
        )
        challenges

  describe "Challenge Golden Answer Tests" do
    it "Should match golden answers" do
      traverse_
        ( \challenge ->
            case challenge.solution of
              Nothing ->
                pure unit
              Just answer -> do
                input <- liftEffect $ readTextFile UTF8 challenge.promptPath
                (challenge.solver input) `shouldEqual` answer

        )
        challenges