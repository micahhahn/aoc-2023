module Data.IntervalMap
  ( Interval(..)
  , IntervalMap(..)
  , inInterval
  , composeIntervalMap
  ) where

import Prelude

import Data.List (List(..), (:))
import Data.List as List
import Data.SortedArray (SortedArray)
import Data.SortedArray as SortedArray
import Data.Tuple.Nested ((/\))

data Interval i a = Interval i i a

derive instance (Eq i, Eq a) => Eq (Interval i a)
derive instance (Ord i, Ord a) => Ord (Interval i a)
instance (Show i, Show a) => Show (Interval i a) where
  show (Interval l r a) = "[" <> show l <> ", " <> show r <> "]: " <> show a <> ""

data IntervalMap i a = IntervalMap (SortedArray (Interval i a))

instance (Eq i, Eq a) => Eq (IntervalMap i a) where
  eq (IntervalMap l) (IntervalMap r) = eq l r

instance (Show i, Show a) => Show (IntervalMap i a) where
  show (IntervalMap m) = show m

inInterval :: forall i a. (Ord i) => i -> Interval i a -> Ordering
inInterval i (Interval l r _) =
  if i < l then
    LT
  else if i > r then
    GT
  else
    EQ

composeIntervalMap :: forall i a. Ord a => Ord i => Ring i => (a -> a -> a) -> IntervalMap i a -> IntervalMap i a -> IntervalMap i a
composeIntervalMap f (IntervalMap from') (IntervalMap to') =
  let
    go from to accum =
      case from /\ to of
        (Interval fLeft fRight fVal : fromOthers) /\ (Interval tLeft tRight tVal : toOthers) ->
          if fLeft < tLeft then
            go (Interval tLeft fRight fVal : fromOthers) to (Interval fLeft (tLeft - one) fVal : accum)
          else if fLeft > tLeft then
            go from (Interval fLeft tRight tVal : toOthers) (Interval tLeft (fLeft - one) tVal : accum)
          else if fRight > tRight then
            go (Interval (tRight + one) fRight fVal : fromOthers) toOthers (Interval tLeft tRight (f fVal tVal) : accum)
          else if fRight < tRight then
            go fromOthers (Interval (fRight + one) tRight tVal : toOthers) (Interval fLeft fRight (f fVal tVal) : accum)
          else
            go fromOthers toOthers (Interval fLeft fRight (f fVal tVal) : accum)

        Nil /\ _ ->
          to <> accum

        _ /\ Nil ->
          from <> accum

  in
    go (List.fromFoldable from') (List.fromFoldable to') Nil
      # SortedArray.fromFoldable
      # IntervalMap