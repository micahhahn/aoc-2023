module Test.Main where

import Prelude

import Challenge (Challenge)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Traversable (traverse_)
import Day1 as Day1
import Day2 as Day2
import Day3 as Day3
import Day4 as Day4
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

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