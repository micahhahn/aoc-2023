module Test.Main where

import Prelude

import Challenge (Challenge)
import Data.Traversable (traverse_)
import Data.String (joinWith)
import Day1 as Day1
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

challenges :: Array Challenge
challenges =
  [ Day1.challenge1
  ]

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Challenge Unit Tests" do
    it "Should pass examples" do
      traverse_
        ( \challenge ->
            challenge.solver (joinWith "\n" challenge.examplePrompt) `shouldEqual` challenge.exampleAnswer
        )
        challenges