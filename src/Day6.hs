{-|
Module:         Day6
Description:    <https://adventofcode.com/2018/day/6 Day 6: Chronal Coordinates>
-}
{-# LANGUAGE TupleSections, TypeApplications, ViewPatterns #-}
module Day6 (day6a, day6b) where

import Control.Arrow ((&&&), (***))
import Control.Monad (ap)
import Data.Either (partitionEithers)
import Data.Ix (range)
import Data.List (sortOn)
import Data.List.NonEmpty (nonEmpty)
import Data.Map.Strict (elems, fromListWith)
import Data.Semigroup (Max(Max, getMax), Min(Min, getMin), sconcat)
import Data.Set (fromList, notMember)

parse :: String -> [(Int, Int)]
parse = map parse1 . lines
  where parse1 s = read $ "(" ++ s ++ ")"

day6a :: String -> Int
day6a (parse -> input@(nonEmpty -> Just input')) =
    maximum $ elems $ fromListWith (+) $ (, 1) <$> filter (`notMember` exclude) include
  where b@((x0, y0), (x1, y1)) = ((getMin *** getMin) *** (getMax *** getMax)) . sconcat $
            ((Min *** Min) &&& (Max *** Max)) <$> input'
        closest (x, y) = uniqueMinIndex [abs (x - x') + abs (y - y') | (x', y') <- input]
        uniqueMinIndex list = case sortOn snd $ zip @Int [0..] list of
            (_, x):(_, y):_ | x == y -> Nothing
            (i, _):_ -> Just i
            _ -> Nothing
        (include, fromList -> exclude) = partitionEithers
          [ (if x == x0 || x == x1 || y == y0 || y == y1 then Right else Left) n
          | ((x, y), Just n) <- zip `ap` map closest $ range b
          ]

day6b :: Int -> String -> Int
day6b limit (parse -> input@(unzip -> (xs@(nonEmpty -> Just xs'), ys@(nonEmpty -> Just ys')))) =
    length $ filter (< limit) [dx + dy | dx <- dxs', dy <- dys']
  where radius = limit `div` length input
        (Min x0, Max x1) = sconcat $ (Min &&& Max) <$> xs'
        (Min y0, Max y1) = sconcat $ (Min &&& Max) <$> ys'
        dxs = [sum [abs $ x - x'| x' <- xs] | x <- [x0 - radius..x1 + radius]]
        dys = [sum [abs $ y - y'| y' <- ys] | y <- [y0 - radius..y1 + radius]]
        dxs' = filter (< limit - minimum dys) dxs
        dys' = filter (< limit - minimum dxs) dys
