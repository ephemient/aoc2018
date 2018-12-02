{-|
Module:         Day2
Description:    <https://adventofcode.com/2018/day/2 Day 2: Inventory Management System>
-}
{-# LANGUAGE TupleSections, ViewPatterns #-}
module Day2 (day2a, day2b) where

import Control.Arrow ((***))
import Data.Function (on)
import Data.List (tails)
import Data.Map.Strict (Map, elems, fromListWith)
import Data.Maybe (catMaybes, isNothing, listToMaybe)
import Data.Monoid (Sum(Sum, getSum), Any(Any))

-- | Counts the number of occurrences of values in a list.
count :: (Ord a) => [a] -> Map a Int
count = fromListWith (+) . map (, 1)

day2a :: String -> Int
day2a = uncurry ((*) `on` getSum) . mconcat . map (check . elems . count) . lines
  where check = (anySum1 *** anySum1) . mconcat . map (\c -> (Any $ c == 2, Any $ c == 3))
        anySum1 (Any True) = Sum 1
        anySum1 _ = Sum 0

day2b :: String -> Maybe String
day2b (lines -> input) = listToMaybe
  [ catMaybes matches
  | one:rest <- tails input
  , two <- rest
  , let matches = zipWith (\a b -> if a == b then Just a else Nothing) one two
  , length (filter isNothing matches) == 1
  ]
