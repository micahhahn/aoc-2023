module Day5
  ( challenge1
  ) where

import Prelude

import Challenge (Challenge)
import Data.Either (Either(..))
import Data.Foldable (minimum)
import Data.Int as Int
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.SortedArray (SortedArray)
import Data.SortedArray as SortedArray
import Parser (Parser, runParser, number, sepBy, newline, space, string, sepEndBy)

-- TODO: Are we overflowing? 

challenge1 :: Challenge
challenge1 =
  { name: "Day 5 Part 1"
  , examplePrompt:
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
  , exampleAnswer: "35"
  , solver: solution1
  , promptPath: "assets/day5.txt"
  , solution: Nothing
  }

seeds :: Parser String (List Int)
seeds = do
  _ <- string "seeds: "
  number `sepBy` space

newtype IntervalMap =
  IntervalMap
    { sourceStart :: Int
    , range :: Int
    , targetStart :: Int
    }

derive instance Eq IntervalMap
instance Ord IntervalMap where
  compare (IntervalMap l) (IntervalMap r) =
    compare l.sourceStart r.sourceStart

instance Show IntervalMap where
  show (IntervalMap x) = "IntervalMap " <> show x

intervalMap :: Parser String IntervalMap
intervalMap = do
  targetStart <- number
  _ <- space
  sourceStart <- number
  _ <- space
  range <- number
  pure $ IntervalMap { targetStart, sourceStart, range }

type CategoryMap = SortedArray IntervalMap

categoryMap :: String -> Parser String CategoryMap
categoryMap categoryName = do
  _ <- string categoryName
  _ <- string " map:\n"
  intervalMaps <- intervalMap `sepEndBy` newline
  pure $ SortedArray.fromFoldable intervalMaps

type Almanac =
  { seeds :: List Int
  , seedToSoil :: CategoryMap
  , soilToFertilizer :: CategoryMap
  , fertilizerToWater :: CategoryMap
  , waterToLight :: CategoryMap
  , lightToTemperature :: CategoryMap
  , temperatureToHumidity :: CategoryMap
  , humidityToLocation :: CategoryMap
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

compareInterval :: Int -> IntervalMap -> Ordering
compareInterval x (IntervalMap { sourceStart, range }) =
  if x < sourceStart then
    LT
  else if x >= sourceStart + range then
    GT
  else
    EQ

withDefault :: forall a. a -> Maybe a -> a
withDefault value maybe =
  case maybe of
    Nothing ->
      value

    Just x ->
      x

translateId :: CategoryMap -> Int -> Int
translateId catMap id =
  SortedArray.findIndexWith (compareInterval id) catMap
    >>= (SortedArray.index catMap)
    <#> (\(IntervalMap { sourceStart, targetStart }) -> (id - sourceStart) + targetStart)
    # withDefault id

seedToLocation :: Almanac -> Int -> Int
seedToLocation { seedToSoil, soilToFertilizer, fertilizerToWater, waterToLight, lightToTemperature, temperatureToHumidity, humidityToLocation } seedId =
  seedId
    # translateId seedToSoil
    # translateId soilToFertilizer
    # translateId fertilizerToWater
    # translateId waterToLight
    # translateId lightToTemperature
    # translateId temperatureToHumidity
    # translateId humidityToLocation

solution1 :: String -> String
solution1 input =
  case runParser input almanac of
    Left err ->
      "Parsing challenge failed: " <> show err

    Right almanac' ->
      almanac'.seeds
        # map (seedToLocation almanac')
        # minimum
        # withDefault 0
        # Int.toStringAs Int.decimal