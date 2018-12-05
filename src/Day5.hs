{-|
Module:         Day5
Description:    <https://adventofcode.com/2018/day/5 Day 5: Alchemical Reaction>
-}
{-# LANGUAGE ViewPatterns #-}
module Day5 (day5a, day5b) where

import Data.Char (isAlpha, isUpper, toUpper)

react :: String -> Int
react = react' "" where
    react' [] (a:as) = react' [a] as
    react' (b:bs) (a:as)
      | isUpper a /= isUpper b && toUpper a == toUpper b = react' bs as
      | otherwise = react' (a:b:bs) as
    react' remainder [] = length remainder

day5a :: String -> Int
day5a = react . filter isAlpha

day5b :: String -> Int
day5b (filter isAlpha -> input) = minimum [react $ filter ((/= c) . toUpper) input | c <- ['A'..'Z']]
