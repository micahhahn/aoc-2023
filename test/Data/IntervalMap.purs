module Test.Data.Interval2
  ( spec
  ) where

import Data.IntervalMap
import Prelude
import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)
import Data.SortedArray as SortedArray

makeI :: Array (Interval Int String) -> IntervalMap Int String
makeI array =
  SortedArray.fromFoldable array
    # IntervalMap

spec :: Spec Unit
spec =
  describe "Data.Interval" do
    describe "composeIntervalMap" do
      it "works for disjoint intervals 1" do
        composeIntervalMap (<>) (makeI []) (makeI [])
          `shouldEqual` (makeI [])

      it "works for disjoint intervals 2" do
        composeIntervalMap (<>) (makeI [ Interval 0 1 "a" ]) (makeI [ Interval 2 3 "b" ])
          `shouldEqual` (makeI [ Interval 0 1 "a", Interval 2 3 "b" ])

      it "works for disjoint intervals 3" do
        composeIntervalMap (<>) (makeI [ Interval 2 3 "b" ]) (makeI [ Interval 0 1 "a" ])
          `shouldEqual` (makeI [ Interval 0 1 "a", Interval 2 3 "b" ])

