{-|
Module:         Day5
Description:    <https://adventofcode.com/2018/day/5 Day 5: Alchemical Reaction>
-}
{-# LANGUAGE ViewPatterns #-}
module Day5 (day5a, day5b) where

import Data.Char (isAlpha, isUpper, toUpper)

react :: String -> Int
react = length . foldr acc ""
  where acc a (b:bs) | isUpper a /= isUpper b && toUpper a == toUpper b = bs
        acc a bs = a:bs

day5a :: String -> Int
day5a = react . filter isAlpha

day5b :: String -> Int
day5b (filter isAlpha -> input) = minimum [react $ filter ((/= c) . toUpper) input | c <- ['A'..'Z']]
