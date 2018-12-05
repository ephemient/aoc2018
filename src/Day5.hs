{-|
Module:         Day5
Description:    <https://adventofcode.com/2018/day/5 Day 5: Alchemical Reaction>
-}
{-# LANGUAGE ViewPatterns #-}
module Day5 (day5a, day5b) where

import Data.Char (isAlpha, toUpper)

react :: String -> String
react = foldr acc ""
  where acc a (b:bs) | a /= b && toUpper a == toUpper b = bs
        acc a bs = a:bs

day5a :: String -> Int
day5a = length . react . filter isAlpha

day5b :: String -> Int
day5b (react . filter isAlpha -> input) = minimum
    [length $ react $ filter ((/= c) . toUpper) input | c <- ['A'..'Z']]
