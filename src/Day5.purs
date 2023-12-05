module Day5
  ( challenge1
  ) where

import Prelude

import Challenge (Challenge)
import Data.Maybe (Maybe(..))
import Parser (Parser, runParser, number, char, sepBy, newline, space, string, sepEndBy)
import Data.Either (Either(..))
import Data.String as String
import Data.List (List)
import Data.SortedArray (SortedArray)
import Effect.Exception (error, throwException)
import Data.SortedArray as SortedArray

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

-- The order of fields is **very** carefully laid out so that we can binary search on `sourceStart` and `range`
newtype IntervalMap =
  IntervalMap
    { sourceStart :: Int
    , range :: Int
    , targetStart :: Int
    }

derive instance Eq IntervalMap
derive instance Ord IntervalMap

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

solution1 :: String -> String
solution1 input =  
  case runParser input almanac of 
    Left err -> 
      "Parsing challenge failed: " <> show err

    Right almanac -> 
      "Worked!"