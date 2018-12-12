{-|
Module:         Day11
Description:    <https://adventofcode.com/2018/day/11 Day 11: Chronal Charge>
-}
{-# LANGUAGE TypeApplications, ViewPatterns #-}
module Day11 (day11a, day11b) where

import Control.Arrow ((***), second)
import Control.Parallel.Strategies (parMap, rpar)
import Data.Array.Unboxed (IArray, Ix, UArray, (!), bounds, listArray, range)
import Data.Char (isDigit)
import Data.List (maximumBy, scanl')
import Data.Ord (comparing)

size :: Int
size = 300

tabulate :: (IArray a e, Integral e) => e -> a (Int, Int) e
tabulate z = listArray ((0, 0), (size, size)) $ scanl build (repeat 0) [1..] >>= take (size + 1)
  where build prev y = zipWith (+) prev $ scanl' (acc y) 0 [1..]
        acc y prev x = prev + let r = x + 10 in ((r * y + z) * r) `div` 100 `mod` 10 - 5

maxBox :: (IArray a e, Num e, Ord e, Ix i, Num i) => a (i, i) e -> i -> ((i, i, i), e)
maxBox a n = maximumBy (comparing snd)
    [((x + 1, y + 1, n), box p) | p@(y, x) <- range $ second (subtract n *** subtract n) $ bounds a]
  where box (y, x) = a ! (y, x) - a ! (y, x + n) - a ! (y + n, x) + a ! (y + n, x + n)

day11a :: String -> String
day11a (tabulate @UArray @Int . read . filter isDigit -> table) = show x ++ "," ++ show y
  where ((x, y, _), _) = maxBox table 3

day11b :: String -> String
day11b (tabulate @UArray @Int . read . filter isDigit -> table) =
    show x ++ "," ++ show y ++ "," ++ show n
  where ((x, y, n), _) = maximumBy (comparing snd) $ parMap rpar (maxBox table) [1..size]
