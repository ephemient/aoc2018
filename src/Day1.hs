{-|
Module:         Day1
Description:    <https://adventofcode.com/2018/day/1 Day 1: Chronal Calibration>
-}
{-# LANGUAGE TransformListComp #-}
module Day1 (day1a, day1b) where

import Control.Applicative ((<|>))
import Data.List (find, scanl', tails)
import Data.IntSet (empty, insert, member)
import Data.Maybe (listToMaybe)
import GHC.Exts (groupWith, sortWith)

parse :: String -> [Int]
parse = map (read . dropWhile (== '+')) . lines

day1a :: String -> Int
day1a = sum . parse

-- | Returns a repeat if found within a single cycle; otherwise, the running sums are grouped by the
-- | remainder modulus total sum, then the first smallest inter-group gap (if any) is returned.
day1b :: String -> Maybe Int
day1b input = beforeLoop <|> afterLoop
  where list = scanl' (+) 0 $ parse input
        beforeLoop = fmap fst . find (uncurry member) . zip list $ scanl (flip insert) empty list
        total = last list
        afterLoop = listToMaybe
          [ endValue
          | (n, z) <- zip [0..] $ tail list
          , then group by z `mod` total using groupWith
          , (gap, startIndex, endValue) <-
              [ (abs $ y - x, startIndex, endValue)
              | (i, x):rest <- tails $ zip n z
              , (j, y) <- rest
              , let (startIndex, endValue) = if (total < 0) == (x < y) then (j, x) else (i, y)
              ]
          , then sortWith by (gap, startIndex)
          ]
