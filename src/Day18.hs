{-|
Module:         Day18
Description:    <https://adventofcode.com/2018/day/18 Day 18: Settlers of The North Pole>
-}
{-# LANGUAGE FlexibleContexts, ViewPatterns #-}
module Day18 (day18) where

import Control.Applicative (liftA2)
import Data.Array.Unboxed (IArray, UArray, (!), (//), assocs, bounds, elems, listArray)
import Data.List (nub)
import Data.Map.Strict (empty, insertLookupWithKey)

parse :: String -> UArray (Int, Int) Char
parse input = listArray ((1, 1), (height, width)) $ concat rows
  where rows = lines input
        height = length rows
        [width] = nub $ length <$> rows

step :: (IArray a Char) => a (Int, Int) Char -> a (Int, Int) Char
step input = input // do
    (coords@(y, x), current) <- assocs input
    let surroundings = (input !) <$> liftA2 (,)
            [max minY $ y - 1..min maxY $ y + 1] [max minX $ x - 1..min maxX $ x + 1]
    updated <- case current of
        '.' | _:_:_:_ <- filter (== '|') surroundings -> "|"
        '|' | _:_:_:_ <- filter (== '#') surroundings -> "#"
        '#' | [] <- filter (== '|') surroundings -> "."
            | [_] <- filter (== '#') surroundings -> "."
        _ -> ""
    return (coords, updated)
  where ((minY, minX), (maxY, maxX)) = bounds input

day18 :: Int -> String -> Int
day18 target (parse -> input) = length (filter (== '|') output) * length (filter (== '#') output)
  where output = elems $ iterateX empty 0 target input
        iterateX _ n goal state | n == goal = state
        iterateX cache n goal state = iterateX cache' (n + 1) goal' $ step state
          where (hit, cache') = insertLookupWithKey (\_ _ a -> a) state n cache
                goal' = maybe target (\m -> n + 1 + (goal - n - 1) `mod` (n - m)) hit
