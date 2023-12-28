module Day5
  ( TemperatureId(..)
  , challenge1
  , challenge2
  , x
  , SeedId(..)
  , FertilizerId(..)
  , LocationId(..)
  ) where

import Data.Tuple.Nested
import Prelude

import Challenge (Challenge)
import Data.Bifunctor (lmap)
import Data.Compactable (compact)
import Data.Either (Either(..))
import Data.Foldable (foldl, minimum)
import Data.Interval.Map (Boundary(..), Interval(..), LeftBoundary(..), Map(..), Piecewise(..), RightBoundary(..))
import Data.Interval.Map as Map
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.SortedArray (SortedArray)
import Data.SortedArray as SortedArray
import Data.String as String
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Parser (Parser, runParser, sepBy, newline, space, string, sepEndBy, bigint)
import Safe.Coerce (coerce, class Coercible)

examplePrompt :: Array String
examplePrompt =
  [ "seeds: 79 14 55 13"
  , ""
  , "seed-to-soil map:"
  , "50 98 2"
  , "52 50 48"
  , ""
  , "soil-to-fertilizer map:"
  , "0 15 37"
  , "37 52 2"
  , "39 0 15"
  , ""
  , "fertilizer-to-water map:"
  , "49 53 8"
  , "0 11 42"
  , "42 0 7"
  , "57 7 4"
  , ""
  , "water-to-light map:"
  , "88 18 7"
  , "18 25 70"
  , ""
  , "light-to-temperature map:"
  , "45 77 23"
  , "81 45 19"
  , "68 64 13"
  , ""
  , "temperature-to-humidity map:"
  , "0 69 1"
  , "1 0 69"
  , ""
  , "humidity-to-location map:"
  , "60 56 37"
  , "56 93 4"
  ]

challenge1 :: Challenge
challenge1 =
  { name: "Day 5 Part 1"
  , examplePrompt: examplePrompt
  , exampleAnswer: "35"
  , solver: solution1
  , promptPath: "assets/day5.txt"
  , solution: Just "84470622"
  }

newtype SeedId = SeedId BigInt

derive newtype instance Show SeedId
derive newtype instance Eq SeedId
derive newtype instance Ord SeedId
derive newtype instance Semiring SeedId
derive newtype instance Ring SeedId
derive instance Newtype SeedId _

newtype SoilId = SoilId BigInt

derive newtype instance Show SoilId
derive newtype instance Eq SoilId
derive newtype instance Ord SoilId
derive newtype instance Semiring SoilId
derive newtype instance Ring SoilId
derive instance Newtype SoilId _

newtype FertilizerId = FertilizerId BigInt

derive newtype instance Show FertilizerId
derive newtype instance Eq FertilizerId
derive newtype instance Ord FertilizerId
derive newtype instance Semiring FertilizerId
derive newtype instance Ring FertilizerId
derive instance Newtype FertilizerId _

newtype WaterId = WaterId BigInt

derive newtype instance Eq WaterId
derive newtype instance Ord WaterId
derive newtype instance Semiring WaterId
derive newtype instance Ring WaterId
derive instance Newtype WaterId _

newtype LightId = LightId BigInt

derive newtype instance Eq LightId
derive newtype instance Ord LightId
derive newtype instance Semiring LightId
derive newtype instance Ring LightId
derive instance Newtype LightId _

newtype TemperatureId = TemperatureId BigInt

derive newtype instance Eq TemperatureId
derive newtype instance Ord TemperatureId
derive newtype instance Semiring TemperatureId
derive newtype instance Ring TemperatureId
derive instance Newtype TemperatureId _

newtype HumidityId = HumidityId BigInt

derive newtype instance Eq HumidityId
derive newtype instance Ord HumidityId
derive newtype instance Semiring HumidityId
derive newtype instance Ring HumidityId
derive instance Newtype HumidityId _

newtype LocationId = LocationId BigInt

derive newtype instance Show LocationId
derive newtype instance Eq LocationId
derive newtype instance Ord LocationId
derive newtype instance Semiring LocationId
derive newtype instance Ring LocationId
derive instance Newtype LocationId _

seeds :: Parser String (List SeedId)
seeds = do
  _ <- string "seeds: "
  (bigint <#> SeedId) `sepBy` space

interval :: forall k v. Newtype k BigInt => Newtype v BigInt => Parser String (Piecewise k v)
interval = do
  targetStart <- bigint
  _ <- space
  sourceStart <- bigint
  _ <- space
  range <- bigint
  let i = Interval (LeftBoundary (wrap sourceStart) Closed) (RightBoundary (wrap (sourceStart + range - one)) Closed)
  let f = (unwrap >>> (+) (targetStart - sourceStart) >>> wrap) /\ (unwrap >>> (+) (sourceStart - targetStart) >>> wrap)
  pure $ Piecewise i f

categoryMap :: forall k v. Newtype k BigInt => Newtype v BigInt => Ord k => String -> Parser String (Map k v)
categoryMap categoryName = do
  _ <- string categoryName
  _ <- string " map:\n"
  intervalMaps <- interval `sepEndBy` newline
  let map = Map (SortedArray.fromFoldable intervalMaps)
  pure $ Map.mapIdentity `Map.compose` map

type Almanac =
  { seeds :: List SeedId
  , seedToSoil :: Map SeedId SoilId
  , soilToFertilizer :: Map SoilId FertilizerId
  , fertilizerToWater :: Map FertilizerId WaterId
  , waterToLight :: Map WaterId LightId
  , lightToTemperature :: Map LightId TemperatureId
  , temperatureToHumidity :: Map TemperatureId HumidityId
  , humidityToLocation :: Map HumidityId LocationId
  }

almanac :: Parser String Almanac
almanac = do
  seeds' <- seeds
  _ <- newline *> newline
  seedToSoil <- categoryMap "seed-to-soil"
  _ <- newline
  soilToFertilizer <- categoryMap "soil-to-fertilizer"
  _ <- newline
  fertilizerToWater <- categoryMap "fertilizer-to-water"
  _ <- newline
  waterToLight <- categoryMap "water-to-light"
  _ <- newline
  lightToTemperature <- categoryMap "light-to-temperature"
  _ <- newline
  temperatureToHumidity <- categoryMap "temperature-to-humidity"
  _ <- newline
  humidityToLocation <- categoryMap "humidity-to-location"
  pure { seeds: seeds', seedToSoil, soilToFertilizer, fertilizerToWater, waterToLight, lightToTemperature, temperatureToHumidity, humidityToLocation }

mkSeedToLocation :: Almanac -> Map SeedId LocationId
mkSeedToLocation { seedToSoil, soilToFertilizer, fertilizerToWater, waterToLight, lightToTemperature, temperatureToHumidity, humidityToLocation } =
  seedToSoil
    `Map.compose` soilToFertilizer
    `Map.compose` fertilizerToWater
    `Map.compose` waterToLight
    `Map.compose` lightToTemperature
    `Map.compose` temperatureToHumidity
    `Map.compose` humidityToLocation

solution1 :: String -> String
solution1 input =
  case runParser input almanac of
    Left err ->
      "Parsing challenge failed: " <> show err

    Right almanac' ->
      let
        seedToLocation = mkSeedToLocation almanac'
      in
        almanac'.seeds
          # map (Map.lookup seedToLocation)
          # compact
          # minimum
          # fromMaybe zero
          # unwrap
          # BigInt.toString

challenge2 :: Challenge
challenge2 =
  { name: "Day 5 Part 2"
  , examplePrompt: examplePrompt
  , exampleAnswer: "46"
  , solver: solution2
  , promptPath: "assets/day5.txt"
  , solution: Nothing
  }

{-
hasGaps :: String -> CategoryMap -> Either String Unit
hasGaps name cat =
  List.fromFoldable cat
    # (\list -> List.zip list (List.drop 1 list))
    # traverse_
        ( \(Tuple (IntervalMap l) (IntervalMap r)) ->
            if l.end == r.start - (BigInt.fromInt 1) then
              pure unit
            else
              Left $ "Gap in " <> name <> " between " <> show l.end <> " and " <> show r.start
        )
-}

{-
composeCategoryMap :: CategoryMap -> CategoryMap -> CategoryMap
composeCategoryMap from' to' =
  let 
      go from to accum = 
        case from /\ to of 
          (IntervalMap f : otherFrom) /\ (IntervalMap t : otherTo) -> 
            if f.start < t.start then
              go (IntervalMap $ f { start = t.start } : otherFrom) to (IntervalMap { start: f.start, end: t.start - one, offset: f.offset } : accum)
            else if t.start < f.start then 
              go from (IntervalMap $ t { start = f.start } : otherTo) (IntervalMap { start: t.start, end: f.start - one, offset: t.offset } : accum) 
            else if f.end > t.end then
              go 
          
  in 
    go (invertCategoryMap from') to'
-}

x ∷ Effect (Either String _)
x = do
  -- input <- readTextFile UTF8 challenge2.promptPath
  let
    input = challenge1.examplePrompt
      # String.joinWith "\n"
  pure $ do
    almanac' <- runParser input almanac # lmap show
    let seedToFertilizer = mkSeedToLocation almanac'
    pure seedToFertilizer

{-
seedToSoil:
  [50, 97]: 2,
  [98, 99]: -48

soilToSeed:
  [50, 51]: 48,
  [52, 99]: -2

soilToFertilizer:
  [0, 14]: 39,
  [15, 51]: -15,
  [52, 53]: -15

True seedToFertilizer: 
  [0, 14]: 39,
  [15, 49]: -15,
  [50, 51]: +2-15
  [52, 97]: +2
  [98, 99]: +2 -15
-}

solution2 :: String -> String
solution2 input =
  input