{-|
Module:         Day20
Description:    <https://adventofcode.com/2018/day/20 Day 20: A Regular Map>
-}
{-# LANGUAGE TypeApplications, ViewPatterns #-}
module Day20 (day20a, day20b) where

import Data.Char (isSpace)
import Data.Graph.Inductive (UGr, level, mkUGraph)
import Data.List (dropWhileEnd)
import Data.Map.Strict ((!), elems, insertLookupWithKey, singleton, size)
import Data.Maybe (fromMaybe)

buildGraph :: String -> UGr
buildGraph (dropWhile (== '^') . dropWhileEnd (== '$') . filter (not . isSpace) -> input) =
    mkUGraph (elems vertices) edges
  where (vertices, edges) = buildGraph' [] (singleton @(Int, Int) (0, 0) 0) [] (0, 0) input
        buildGraph' stack vs es p ('(':ds) = buildGraph' (p:stack) vs es p ds
        buildGraph' stack@(p:_) vs es _ ('|':ds) = buildGraph' stack vs es p ds
        buildGraph' (p:stack) vs es _ (')':ds) = buildGraph' stack vs es p ds
        buildGraph' stack vs es p@(x, y) (d:ds)
          | d `elem` "NESW" = buildGraph' stack vs' ((vs ! p, v):es) p' ds
          where p' = case d of
                    'N' -> (x, y - 1)
                    'E' -> (x + 1, y)
                    'S' -> (x, y + 1)
                    'W' -> (x - 1, y)
                    _ -> error $ "not a valid direction: " ++ [d]
                (fromMaybe (size vs) -> v, vs') =
                    insertLookupWithKey (\_ _ a -> a) p' (size vs) vs
        buildGraph' [] vs es _ "" = (vs, es)
        buildGraph' _ _ _ _ _ = error "mismatched parentheses"

day20a :: String -> Int
day20a = maximum . fmap snd . level 0 . buildGraph

day20b :: String -> Int
day20b = length . filter (>= 1000) . fmap snd . level 0 . buildGraph
