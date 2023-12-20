module Data.Interval.Map2
  ( inInterval
  , Interval(..)
  , LeftBoundary(..)
  , RightBoundary(..)
  , Boundary(..)
  , whole
  ) where

import Prelude
import Data.SortedArray (SortedArray)
import Data.SortedArray as SortedArray
import Data.Ordering (invert)
import Data.Maybe (Maybe)
import Data.List as List
import Data.Tuple.Nested ((/\), type (/\))

data Boundary
  = Closed
  | Open

derive instance Eq Boundary
derive instance Ord Boundary

data LeftBoundary k
  = NegInf
  | LeftBoundary k Boundary

derive instance Eq k => Eq (LeftBoundary k)
derive instance Ord k => Ord (LeftBoundary k)

instance (Show k) => Show (LeftBoundary k) where
  show NegInf = "(-∞"
  show (LeftBoundary left Open) = "(" <> show left
  show (LeftBoundary left Closed) = "[" <> show left

derive instance Functor LeftBoundary

data RightBoundary k
  = RightBoundary k Boundary
  | PosInf

derive instance Eq k => Eq (RightBoundary k)
instance Ord k => Ord (RightBoundary k) where
  compare PosInf PosInf = EQ
  compare PosInf _ = GT
  compare _ PosInf = LT
  compare (RightBoundary left leftBoundary) (RightBoundary right rightBoundary) =
    compare left right <> (invert $ compare leftBoundary rightBoundary)

instance (Show k) => Show (RightBoundary k) where
  show (RightBoundary right Closed) = show right <> "]"
  show (RightBoundary right Open) = show right <> ")"
  show PosInf = "∞)"

derive instance Functor RightBoundary

data Interval k = Interval (LeftBoundary k) (RightBoundary k)

derive instance Eq k => Eq (Interval k)
derive instance Ord k => Ord (Interval k)

instance (Show k) => Show (Interval k) where
  show (Interval leftBoundary rightBoundary) =
    show leftBoundary <> ", " <> show rightBoundary

derive instance Functor Interval

type Bijection a b = (a -> b) /\ (b -> a)

forward :: forall a b. Bijection a b -> (a -> b)
forward (f /\ _) = f

backward :: forall a b. Bijection a b -> (b -> a)
backward (_ /\ f) = f

composeBijection :: forall a b c. Bijection a b -> Bijection b c -> Bijection a c
composeBijection (fForward /\ fBackward) (gForward /\ gBackward) =
  fForward >>> gForward /\ gBackward >>> fBackward

data Piecewise a b = Piecewise (Interval a) (Bijection a b)

instance Eq a => Eq (Piecewise a b) where
  eq (Piecewise leftI _) (Piecewise rightI _) =
    eq leftI rightI

instance Ord a => Ord (Piecewise a b) where
  compare (Piecewise leftInterval _) (Piecewise rightInterval _) =
    compare leftInterval rightInterval

instance (Show a, Show b) => Show (Piecewise a b) where
  show (Piecewise interval bijection) =
    show interval <> " ==> " <> show (map (forward bijection) interval)

data Map k v = Map (SortedArray (Piecewise k v))

-- | Tests if value `k` is contained in the interval. 
-- |
-- | Returns an `Ordering` so that it can be used in binary search
inInterval :: forall k. (Ord k) => k -> Interval k -> Ordering
inInterval k (Interval leftBoundary rightBoundary) =
  if LeftBoundary k Open <= leftBoundary then
    LT
  else if RightBoundary k Open >= rightBoundary then
    GT
  else
    EQ

whole :: forall k. Interval k
whole = Interval NegInf PosInf

lookup :: forall k v. (Ord k) => Map k v -> k -> Maybe v
lookup (Map piecewises) key =
  SortedArray.findIndexWith (\(Piecewise interval _) -> inInterval key interval) piecewises
    >>= (SortedArray.index piecewises)
    <#> (\(Piecewise _ f') -> forward f' key)

lookupLeft :: forall k v. (Ord k) => Map k v -> LeftBoundary k -> Int
lookupLeft _ NegInf = 0
lookupLeft (Map piecewises) (LeftBoundary key b) =
  SortedArray.findIndexWith (\(Piecewise interval _) -> inInterval key interval) piecewises

lookupRight :: forall k v. (Ord k) => Map k v -> RightBoundary k -> Int
lookupRight (Map pieces) PosInf = SortedArray.length pieces - 1
lookupRight (Map pieces) (RightBoundary key b) =
  SortedArray.findIndexWith (\(Piecewise interval _) -> inInterval key interval) pieces

composeInterval :: forall a b c. Ord a => Ord b => Piecewise a b -> Map b c -> Map a c
composeInterval (Piecewise (Interval leftBoundary rightBoundary) bijection) (Map gPieces) =
  let newLeftBoundary = map (forward bijection) leftBoundary
      newRightBounday =  map (forward bijection) rightBoundary
      x = SortedArray.slice (lookupLeft gPieces newLeftBoundary) (lookupRight gPieces newRightBounday)
  in x

compose :: forall a b c. Ord a => Ord b => Map a b -> Map b c -> Map a c
compose (Map fPieces) (Map gPieces) =
  let go (Piecewise (Interval leftBoundary rightBoundary) bijection) = 
        map (forward bijection) leftBoundary
        map (forward bijection) rightBoundary

  in
  List.fromFoldable fPieces
    # map go