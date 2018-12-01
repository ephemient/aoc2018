{-|
Module:         Day1
Description:    <http://adventofcode.com/2018/day/1 Day 1: Chronal Calibration>
-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day1 (day1a, day1b) where

import Data.List (scanl')
import Data.Set (empty, insert, member)

parse :: String -> [Int]
parse = map (read . dropWhile (== '+')) . lines

day1a :: String -> Int
day1a = sum . parse

day1b :: String -> Int
day1b input = fst . head . filter (uncurry member) . zip list $ scanl (flip insert) empty list
  where list = scanl' (+) 0 $ cycle $ parse input
