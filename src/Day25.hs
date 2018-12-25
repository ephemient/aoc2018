{-|
Module:         Day25
Description:    <https://adventofcode.com/2018/day/25 Day 25: Four-Dimensional Adventure>
-}
module Day25 (day25a) where

import Data.Graph.Inductive (Gr,mkGraph, scc)

day25a :: String -> Int
day25a input = length $ scc gr
  where nodes = zip [0..] [read $ '(' : line ++ ")" | line <- lines input]
        gr = mkGraph nodes
          [ (i, j, d)
          | (i, (x0, y0, z0, t0)) <- nodes
          , (j, (x1, y1, z1, t1)) <- nodes
          , let d = abs (x0 - x1) + abs (y0 - y1) + abs (z0 - z1) + abs (t0 - t1)
          , d <= 3
          ] :: Gr (Int, Int, Int, Int) Int
