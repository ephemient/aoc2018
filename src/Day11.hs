{-|
Module:         Day11
Description:    <https://adventofcode.com/2018/day/11 Day 11: Chronal Charge>
-}
{-# LANGUAGE TypeApplications, ViewPatterns #-}
module Day11 (day11a, day11b) where

import Data.Char (isDigit)
import Data.Function (on)
import Data.List (maximumBy, scanl', tails)
import Data.Ord (comparing)

size :: Int
size = 300

tabulate :: (Integral a) => a -> [[a]]
tabulate z = scanl build (repeat 0) [1..]
  where build prev y = zipWith (+) prev $ scanl' (acc y) 0 [1..]
        acc y prev x = prev + let r = x + 10 in ((r * y + z) * r) `div` 100 `mod` 10 - 5

day11a :: String -> String
day11a (tabulate . read @Int . filter isDigit -> table) = show maxX ++ "," ++ show maxY
  where ((maxX, maxY), _) = maximumBy (comparing snd)
          [ ((x, y), a - b - c + d)
          | (y, row:_:_:row':_) <- zip [1..size - 2] $ tails table
          , (x, a:_:_:b:_, c:_:_:d:_) <- (zip3 [1..size - 2] `on` tails) row row'
          ]

day11b :: String -> String
day11b (tabulate . read @Int . filter isDigit -> table) =
    show maxX ++ "," ++ show maxY ++ "," ++ show maxN
  where ((maxX, maxY, maxN), _) = maximumBy (comparing snd)
          [ ((x, y, z + 1), a - b - c + d)
          | (y, row:table') <- zip [1..size] $ tails table
          , (x, a:row') <- zip [1..size] $ tails row
          , z <- [0..size - max x y]
          , let b = row' !! z
                c:row'' = drop (x - 1) $ table' !! z
                d = row'' !! z
          ]
