{-|
Module:         Day20
Description:    <https://adventofcode.com/2018/day/20 Day 20: A Regular Map>
-}
{-# LANGUAGE BangPatterns, FlexibleContexts, TupleSections, ViewPatterns #-}
module Day20 (day20a, day20b) where

import Control.Arrow (first, second)
import Control.Monad.State.Strict (evalState, foldM, state)
import Data.Char (isSpace)
import Data.IntSet (IntSet, (\\))
import qualified Data.IntSet as IntSet (empty, singleton, toList, union)
import Data.List (dropWhileEnd)
import Data.Map.Strict (Map, findWithDefault, insertLookupWithKey, insertWith, size)
import qualified Data.Map.Strict as Map (empty, singleton)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq((:<|)), (><))
import qualified Data.Sequence as Seq (fromList, singleton)
import Data.Set (mapMonotonic)
import qualified Data.Set as Set (singleton, union)

buildGraph :: String -> Map Int IntSet
buildGraph (dropWhile (== '^') . dropWhileEnd (== '$') . filter (not . isSpace) -> input) = graph
  where start = (0, 0) :: (Int, Int)
        ([_], graph) = flip evalState (Map.singleton start 0) $
            foldM acc ([Set.singleton start], Map.empty) input
        toKey p = state $ \m@(size -> n) ->
            first (fromMaybe n) $ insertLookupWithKey (\_ _ a -> a) p n m
        acc (p:ps, !m) 'N' = (mapMonotonic north p : ps,) <$> foldM (move north) m p
        acc (p:ps, !m) 'E' = (mapMonotonic east p : ps,) <$> foldM (move east) m p
        acc (p:ps, !m) 'S' = (mapMonotonic south p : ps,) <$> foldM (move south) m p
        acc (p:ps, !m) 'W' = (mapMonotonic west p : ps,) <$> foldM (move west) m p
        acc (p:ps, !m) '(' = return (p:p:p:ps, m)
        acc (p:q:r:ps, !m) '|' = return (q : q : Set.union p r : ps, m)
        acc (p:_:q:ps, !m) ')' = return (Set.union p q : ps, m)
        (north, east, south, west) = (second pred, first succ, second succ, first pred)
        move f m p = do
            u <- toKey p
            v <- toKey $ f p
            return $ insertWith IntSet.union u (IntSet.singleton v) $
                insertWith IntSet.union v (IntSet.singleton u) m

bfs :: Int -> Map Int IntSet -> [Int]
bfs start graph = bfs' IntSet.empty $ Seq.singleton (0, start)
  where bfs' seen ((depth, v) :<| queue) = depth : bfs' (IntSet.union seen next)
            (queue >< Seq.fromList ((depth + 1,) <$> IntSet.toList next))
          where next = findWithDefault IntSet.empty v graph \\ seen
        bfs' _ _ = []

day20a :: String -> Int
day20a = maximum . bfs 0 . buildGraph

day20b :: String -> Int
day20b = length . dropWhile (< 1000) . bfs 0 . buildGraph
