module Test.Data.Interval.Map
  ( spec
  ) where

import Data.Interval.Map (Interval(..), Bijection, Map(..), compose)
import Prelude
import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)
import Data.Tuple.Nested ((/\), type (/\))
import Data.SortedArray as SortedArray

makeI :: Array (Interval Int Int) -> Map Int Int
makeI array =
  SortedArray.fromFoldable array
    # (\a -> Map a (identity /\ identity))

makeB :: Int -> Bijection Int Int
makeB x = (\y -> y + x) /\ (\y -> y - x)

norm :: Map Int Int -> Array ((Int /\ Int) /\ Int)
norm (Map intervals _) =
  SortedArray.map (\(Interval r (f /\ _)) -> r /\ f zero) intervals

spec :: Spec Unit
spec =
  describe "Data.Interval.Map" do
    describe "compose" do
      it "works for disjoint intervals 1" do
        compose (makeI []) (makeI [])
          # norm
          # shouldEqual (norm $ makeI [])

      it "works for disjoint intervals 2" do
        compose (makeI [ Interval (0 /\ 1) (makeB 1) ]) (makeI [ Interval (3 /\ 4) (makeB 2) ])
          # norm
          # shouldEqual (norm $ makeI [ Interval (0 /\ 1) (makeB 1), Interval (3 /\ 4) (makeB 2) ])

      it "works for disjoint intervals 3" do
        compose (makeI [ Interval (3 /\ 4) (makeB 2) ]) (makeI [ Interval (0 /\ 1) (makeB 1) ])
          # norm
          # shouldEqual (norm $ makeI [ Interval (0 /\ 1) (makeB 1), Interval (3 /\ 4) (makeB 2) ])

      it "works for identical intervals" do
        compose (makeI [ Interval (0 /\ 1) (makeB 1) ]) (makeI [ Interval (1 /\ 2) (makeB 1) ])
          # norm
          # shouldEqual (norm $ makeI [ Interval (0 /\ 0) (makeB 2), Interval (1 /\ 1) (makeB 2), Interval (2 /\ 2) (makeB 1) ])

      it "works for overlapping intervals 1" do
        compose (makeI [ Interval (0 /\ 1) (makeB 1) ]) (makeI [ Interval (2 /\ 3) (makeB 2) ])
          # norm
          # shouldEqual
              ( norm $ makeI
                  [ Interval (0 /\ 0) (makeB 1)
                  , Interval (1 /\ 1) (makeB 3)
                  , Interval (2 /\ 3) (makeB 2)
                  ]
              )

      it "works for overlapping intervals 2" do
        compose (makeI [ Interval (1 /\ 2) (makeB (-1)) ]) (makeI [ Interval (0 /\ 1) (makeB 1) ])
          # norm
          # shouldEqual (norm $ makeI [ Interval (0 /\ 0) (makeB 1), Interval (1 /\ 2) (makeB 0) ])
