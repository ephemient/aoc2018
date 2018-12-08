{-|
Module:         Day7
Description:    <https://adventofcode.com/2018/day/7 Day 7: The Sum of Its Parts>
-}
{-# LANGUAGE TupleSections #-}
module Day7 (day7a, day7b) where

import Data.Char (ord)
import Data.List (sort, unfoldr)
import Data.Map (Map)
import qualified Data.Map as Map (assocs, delete, filter, fromListWith, lookupMin, map, null)
import Data.Set (Set)
import qualified Data.Set as Set (delete, empty, null, singleton, union)

parse :: String -> Map Char (Set Char)
parse input = Map.fromListWith Set.union $ concat
    [[(line !! 5, Set.empty), (line !! 36, Set.singleton $ line !! 5)] | line <- lines input]

day7a :: String -> String
day7a = unfoldr f . parse
  where f deps = case Map.lookupMin $ Map.filter Set.null deps of
            Just (k, _) -> Just (k, Map.map (Set.delete k) $ Map.delete k deps)
            _ -> if Map.null deps then Nothing else error "unsatisfied dependencies"

day7b :: Int -> Int -> String -> Int
day7b baseCost workers = last . unfoldr f . ([],) . parse
  where cost c = baseCost + ord c - ord 'A' + 1
        f ((t, k):ready, deps) = Just (t, g t ready $ Map.map (Set.delete k) $ Map.delete k deps)
        f ([], deps) = if Map.null deps then Nothing else Just (0, g 0 [] deps)
        g t ready deps = (, deps) $ sort $ ready ++ take (workers - length ready)
            [(t + cost k, k) | (k, a) <- Map.assocs deps, k `notElem` map snd ready, Set.null a]
