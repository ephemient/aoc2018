{-|
Module:         Day11
Description:    <https://adventofcode.com/2018/day/11 Day 11: Chronal Charge>
-}
{-# LANGUAGE TypeApplications, ViewPatterns #-}
module Day11 (day11a, day11b) where

import Data.Array.Unboxed (IArray, Ix, UArray, (!), listArray, range)
import Data.Char (isDigit)
import Data.List (maximumBy, scanl')
import Data.Ord (comparing)

size :: Int
size = 300

tabulate :: (IArray a e, Integral e) => e -> a (Int, Int) e
tabulate z = listArray ((0, 0), (size, size)) $ scanl build (repeat 0) [1..] >>= take (size + 1)
  where build prev y = zipWith (+) prev $ scanl' (acc y) 0 [1..]
        acc y prev x = prev + let r = x + 10 in ((r * y + z) * r) `div` 100 `mod` 10 - 5

box :: (IArray a e, Num e, Ix i, Num i) => a (i, i) e -> (i, i) -> i -> e
box a (y, x) n = a ! (y, x) - a ! (y, x + n) - a ! (y + n, x) + a ! (y + n, x + n)

day11a :: String -> String
day11a (tabulate @UArray @Int . read . filter isDigit -> table) = show maxX ++ "," ++ show maxY
  where ((maxX, maxY), _) = maximumBy (comparing snd)
            [((x + 1, y + 1), box table p 3) | p@(y, x) <- range ((0, 0), (size - 3, size - 3))]

day11b :: String -> String
day11b (tabulate @UArray @Int . read . filter isDigit -> table) =
    show maxX ++ "," ++ show maxY ++ "," ++ show maxN
  where ((maxX, maxY, maxN), _) = maximumBy (comparing snd)
          [ ((x + 1, y + 1, n), box table p n)
          | p@(y, x) <- range ((0, 0), (size - 1, size - 1))
          , n <- [1..size - max x y]
          ]
