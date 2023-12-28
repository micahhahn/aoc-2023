module Test.Data.Interval.Map
  ( spec
  ) where

import Data.Interval.Map
import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Enum (enumFromTo, class Enum, succ, pred)
import Data.Maybe (fromMaybe)
import Data.SortedArray as SortedArray
import Data.Traversable (traverse_)
import Data.Tuple.Nested ((/\))
import Effect.Exception (Error, error)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (expectError, shouldEqual)

fail :: forall m v. MonadThrow Error m => String -> m v
fail = throwError <<< error

normalizeLeft :: forall k m. MonadThrow Error m => Enum k => Show k => LeftBoundary k -> m k
normalizeLeft negInf@NegInf = fail $ "Unexpected " <> show negInf
normalizeLeft (LeftBoundary k Closed) = pure k
normalizeLeft (LeftBoundary k Open) = pure $ fromMaybe k (succ k)

normalizeRight :: forall k m. MonadThrow Error m => Enum k => Show k => RightBoundary k -> m k
normalizeRight posInf@PosInf = fail $ "Unexpected " <> show posInf
normalizeRight (RightBoundary k Closed) = pure k
normalizeRight (RightBoundary k Open) = pure $ fromMaybe k (pred k)

-- | A specialized kind of equality for testing Maps given the following conditions:
-- | - Both maps are finite
-- | - The key type is enumerable between the boundaries
mapShouldEqual :: forall m k v. MonadThrow Error m => Enum k => Eq v => Show k => Show v => Map k v -> Map k v -> m Unit
mapShouldEqual map1 map2 = do
  left <- normalizeLeft $ min (minBoundary map1) (minBoundary map2)
  right <- normalizeRight $ max (maxBoundary map1) (maxBoundary map2)
  traverse_
    ( \input ->
        let
          output1 = lookup map1 input
          output2 = lookup map2 input
        in
          when (output1 /= output2) $ fail $ "At " <> show input <> ": " <> show output1 <> " ≠ " <> show output2
    )
    (enumFromTo left right :: Array k)

spec :: Spec Unit
spec =
  describe "Data.Interval.Map2" do
    describe "inInterval" do
      it "everything is in the whole range" do
        inInterval 0 whole
          # shouldEqual EQ

      it "left open range" do
        inInterval 0 (Interval (LeftBoundary 0 Open) PosInf)
          # shouldEqual LT

      it "left closed range" do
        inInterval 0 (Interval (LeftBoundary 0 Closed) PosInf)
          # shouldEqual EQ

      it "right open range" do
        inInterval 1 (Interval NegInf (RightBoundary 1 Open))
          # shouldEqual GT

      it "right closed range" do
        inInterval 1 (Interval NegInf (RightBoundary 1 Closed))
          # shouldEqual EQ

    describe "mapShouldEqual" do
      it "should pass for simple maps" do
        let map = Map (SortedArray.singleton (Piecewise (Interval (LeftBoundary 1 Closed) (RightBoundary 3 Closed)) ((_ + 1) /\ (_ - 1))))
        map `mapShouldEqual` map

      it "should pass for disjoint maps" do
        let bijection = (_ + 1) /\ (_ - 1)

        let
          disjointPieces =
            [ Piecewise (Interval (LeftBoundary 1 Closed) (RightBoundary 2 Open)) bijection
            , Piecewise (Interval (LeftBoundary 2 Closed) (RightBoundary 3 Closed)) bijection
            ]

        let
          pieces =
            [ Piecewise (Interval (LeftBoundary 1 Closed) (RightBoundary 3 Closed)) bijection ]

        Map (SortedArray.fromFoldable disjointPieces)
          `mapShouldEqual` Map (SortedArray.fromFoldable pieces)

      it "should fail for mismatched intervals (1)" do
        let bijection = (_ + 1) /\ (_ - 1)
        let pieces1 = [ Piecewise (Interval (LeftBoundary 1 Closed) (RightBoundary 2 Open)) bijection ]
        let pieces2 = [ Piecewise (Interval (LeftBoundary 1 Closed) (RightBoundary 2 Closed)) bijection ]
        Map (SortedArray.fromFoldable pieces1) `mapShouldEqual` Map (SortedArray.fromFoldable pieces2)
          # expectError

      it "should fail for mismatched intervals (2)" do
        let bijection = (_ + 1) /\ (_ - 1)
        let pieces1 = [ Piecewise (Interval (LeftBoundary 1 Open) (RightBoundary 2 Closed)) bijection ]
        let pieces2 = [ Piecewise (Interval (LeftBoundary 1 Closed) (RightBoundary 2 Closed)) bijection ]
        Map (SortedArray.fromFoldable pieces1) `mapShouldEqual` Map (SortedArray.fromFoldable pieces2)
          # expectError