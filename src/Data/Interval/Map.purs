module Data.Interval.Map
  ( compose
  , invert
  , inInterval
  , lookup
  , Map(..)
  , Interval(..)
  , Bijection
  ) where

import Prelude

import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.SortedArray (SortedArray)
import Data.SortedArray as SortedArray
import Data.Tuple (swap, fst, snd)
import Data.Tuple.Nested ((/\), type (/\))

type Bijection a b = (a -> b) /\ (b -> a)

data Interval k v = Interval (k /\ k) (Bijection k v)

data Boundary
  = Open
  | Closed

data Extended r
  = NegInf
  | Finite Boundary r
  | PosInf

data Map k v = Map (SortedArray (Interval k v)) (Bijection k v)

instance Eq k => Eq (Interval k v) where
  eq (Interval l _) (Interval r _) =
    eq l r

instance Ord k => Ord (Interval k v) where
  compare (Interval l _) (Interval r _) =
    compare l r

instance (Show k, Show v) => Show (Interval k v) where
  show (Interval (l /\ r) (f /\ _)) =
    "[" <> show l <> ", " <> show r <> "] -> [" <> show (f l) <> ", " <> show (f r) <> "]"

instance (Show k, Show v) => Show (Map k v) where
  show (Map intervals _) =
    show intervals

inInterval :: forall k v. (Ord k) => k -> Interval k v -> Ordering
inInterval k (Interval (l /\ r) _) =
  if k < l then
    LT
  else if k > r then
    GT
  else
    EQ

withDefault :: forall a. a -> Maybe a -> a
withDefault value maybe =
  case maybe of
    Nothing ->
      value

    Just y ->
      y

lookup :: forall k v. (Ord k) => Map k v -> k -> v
lookup (Map intervals f) key =
  SortedArray.findIndexWith (inInterval key) intervals
    >>= (SortedArray.index intervals)
    <#> (\(Interval _ f') -> forward f' key)
    # withDefault (forward f key)

invertInterval :: forall k v. Interval k v -> Interval v k
invertInterval (Interval (l /\ r) (f /\ unF)) =
  Interval (f l /\ f r) (unF /\ f)

invert :: forall k v. (Ord v) => Map k v -> Map v k
invert (Map intervals f) =
  intervals
    # SortedArray.map invertInterval
    # SortedArray.fromFoldable
    # \array -> Map array (swap f)

composeBijection :: forall a b c. Bijection a b -> Bijection b c -> Bijection a c
composeBijection (fForward /\ fBackward) (gForward /\ gBackward) =
  fForward >>> gForward /\ gBackward >>> fBackward

forward :: forall a b. Bijection a b -> (a -> b)
forward = fst

backward :: forall a b. Bijection a b -> (b -> a)
backward = snd

compose :: forall a b c. Ord a => Ord b => Ring a => Ring b => Map a b -> Map b c -> Map a c
compose ff@(Map _ f) gg@(Map gIntervals _) =
  let
    identityBijection = identity /\ identity

    precompose =
      SortedArray.map (\(Interval (l /\ r) _) -> Interval (backward f l /\ backward f r) identityBijection) gIntervals
        # SortedArray.fromFoldable
        # \array -> Map array identityBijection

  in
    (precompose `composeHelper` ff) `composeHelper` gg

--- [ (+1) ]
---  [      ]

---      [   ]

-- Use Enum for succ and prev? 

-- We need to project the f intervals forward (in order) and use binary search to locate them

composeHelper :: forall a b c. Ord a => Ord b => Ring a => Ring b => Map a b -> Map b c -> Map a c
composeHelper (Map fIntervals f) (Map gIntervals g) =
  let
    liftF (Interval i f') =
      Interval i (composeBijection f' g)

    liftG (Interval (l /\ r) g') =
      Interval (backward f l /\ backward f r) (composeBijection f g')

    go Nil todoG accum =
      map liftG todoG <> accum

    go todoF Nil accum =
      map liftF todoF <> accum

    go
      todoF@(fInterval@(Interval (fLeft /\ fRight) f') : fOthers)
      todoG@(gInterval@(Interval (gLeft /\ gRight) g') : gOthers)
      accum =
      if forward f' fRight < gLeft then
        go fOthers todoG (liftF fInterval : accum)
      else if forward f' fLeft > gRight then
        go todoF gOthers (liftG gInterval : accum)
      else if forward f' fLeft < gLeft then
        go (Interval (backward f' gLeft /\ fRight) f' : fOthers) todoG (Interval (fLeft /\ backward f' (gLeft - one)) (composeBijection f' g) : accum)
      else if forward f' fLeft > gLeft then
        go todoF (Interval (forward f' fLeft /\ gRight) g' : gOthers) (Interval (backward f gLeft /\ (fLeft - one)) (composeBijection f g') : accum)
      else if forward f' fRight > gRight then
        go (Interval (backward f' (gRight + one) /\ fRight) f' : fOthers) gOthers (Interval (fLeft /\ (backward f' gRight)) (composeBijection f' g') : accum)
      else if forward f' fRight < gRight then
        go fOthers (Interval (forward f' (fRight + one) /\ gRight) g' : gOthers) (Interval (fLeft /\ fRight) (composeBijection f' g') : accum)
      else
        go fOthers gOthers (Interval (fLeft /\ fRight) (composeBijection f' g') : accum)

  in
    go (List.fromFoldable fIntervals) (List.fromFoldable gIntervals) Nil
      # SortedArray.fromFoldable
      # \array -> Map array (composeBijection f g)
