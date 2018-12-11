{-|
Module:         Day11
Description:    <https://adventofcode.com/2018/day/11 Day 11: Chronal Charge>
-}
{-# LANGUAGE FlexibleContexts, TypeApplications, ViewPatterns #-}
module Day11 (day11a, day11b) where

import Control.Arrow ((***), second)
import Data.Array.Unboxed (UArray, (!), assocs, bounds, listArray, range, rangeSize)
import Data.Char (isDigit)
import Data.List (maximumBy, unfoldr)
import Data.Ord (comparing)

dimens :: ((Int, Int), (Int, Int))
dimens = ((1, 1), (300, 300))

power :: (Integral a) => a -> (a, a) -> a
power z (x, y) = let r = x + 10 in ((r * y + z) * r) `div` 100 `mod` 10 - 5

day11a :: String -> String
day11a (read . filter isDigit -> z) = show maxX ++ "," ++ show maxY
  where table = listArray dimens $ power z <$> range dimens :: UArray (Int, Int) Int
        ((maxX, maxY), _) = maximumBy (comparing snd)
          [ (p, sum [table ! q | q <- range ((x, y), (x + 2, y + 2))])
          | p@(x, y) <- range ((1, 1), (298, 298))
          ]

day11b :: String -> String
day11b (read . filter isDigit -> z) = show maxX ++ "," ++ show maxY ++ "," ++ show @Int maxN
  where table = listArray dimens $ power z <$> range dimens :: UArray (Int, Int) Int
        (maxN, ((maxX, maxY), _)) = maximumBy (comparing $ snd . snd) $ zip [1..] $
            maximumBy (comparing snd) . assocs <$> table : unfoldr grow (1, table)
        grow (n, prev)
          | rangeSize dimens' <= 0 = Nothing
          | otherwise = Just (next, (n + 1, next))
          where dimens' = second (pred *** pred) $ bounds prev
                next = listArray dimens'
                  [ prev ! p +
                        sum [table ! (x + n, y + i) | i <- [0..n - 1]] +
                        sum [table ! (x + i, y + n) | i <- [0..n]]
                  | p@(x, y) <- range dimens'
                  ]
