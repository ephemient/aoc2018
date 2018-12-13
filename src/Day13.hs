{-|
Module:         Day13
Description:    <https://adventofcode.com/2018/day/13 Day 13: Mine Cart Madness>
-}
{-# LANGUAGE FlexibleContexts, TypeApplications, ViewPatterns #-}
module Day13 (day13a, day13b) where

import Control.Arrow ((***), first, second)
import Control.Monad.Cont (callCC, runCont)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Loops (iterateM_, iterateUntilM)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map (delete, empty, findMin, fromList, insert, lookup, member, minViewWithKey, size)

data Crossing = Reflect | Reflect' | Spin

data Orientation = North | West | South | East

move :: Num a => Orientation -> (a, a) -> (a, a)
move North = first (subtract 1)
move West = second (subtract 1)
move South = first (+ 1)
move East = second (+ 1)

rotL, rotR, reflect, reflect' :: Orientation -> Orientation
rotL North = West; rotL West = South; rotL South = East; rotL East = North
rotR West = North; rotR South = West; rotR East = South; rotR North = East
reflect North = East; reflect East = North; reflect West = South; reflect South = West
reflect' North = West; reflect' West = North; reflect' South = East; reflect' East = South

parse :: (Integral i) => String -> (Map (i, i) Crossing, Map (i, i) (Orientation, Int))
parse input = (Map.fromList *** Map.fromList) . partitionEithers $ do
    (y, line) <- zip [0..] $ lines input
    (x, c) <- zip [0..] line
    case c of
        '/' -> [Left ((y, x), Reflect)]
        '\\' -> [Left ((y, x), Reflect')]
        '+' -> [Left ((y, x), Spin)]
        '^'  -> [Right ((y, x), (North, 0))]
        '<'  -> [Right ((y, x), (West, 0))]
        'v'  -> [Right ((y, x), (South, 0))]
        '>'  -> [Right ((y, x), (East, 0))]
        _ -> []

step :: (Integral i, Monad m) => ((i, i) -> m ()) -> Map (i, i) Crossing -> Map (i, i) (Orientation, Int) -> m (Map (i, i) (Orientation, Int))
step notifyCollision crossings = step' Map.empty
  where step' past (Map.minViewWithKey -> Just ((k, (o, n)), future))
          | Map.member k' past || Map.member k' future
          = notifyCollision k' >> (step' `on` Map.delete k') past future
          | otherwise = step' (Map.insert k' v past) future
          where k' = move o k
                v = case Map.lookup k' crossings of
                    Nothing -> (o, n)
                    Just Reflect -> (reflect o, n)
                    Just Reflect' -> (reflect' o, n)
                    Just Spin -> (spins !! (n `mod` length spins) $ o, n + 1)
        step' past _ = return past
        spins = [rotL, id, rotR]

day13a :: String -> String
day13a (parse @Int -> (crossings, cars)) = show x ++ "," ++ show y
  where (y, x) = runCont (callCC $ \f -> iterateM_ (step f crossings) cars) id

day13b :: String -> String
day13b (parse @Int -> (crossings, cars)) = show x ++ "," ++ show y
  where ((y, x), _) = Map.findMin $ runIdentity $
            iterateUntilM ((== 1) . Map.size) (step (const $ return ()) crossings) cars
