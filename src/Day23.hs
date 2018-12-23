{-|
Module:         Day23
Description:    <https://adventofcode.com/2018/day/23 Day 23: Experimental Emergency Teleportation>
-}
{-# LANGUAGE FlexibleContexts, RecordWildCards, TypeApplications, ViewPatterns #-}
module Day23 (day23a, day23b) where

import Control.Monad (ap, when)
import Control.Monad.State.Strict (MonadState, execState, get, put)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet (singleton, size, union, unions)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (delete, elems, fromListWith, mapKeys, mapKeysMonotonic, mapKeysWith, minViewWithKey, partitionWithKey)
import Data.Maybe (fromJust, listToMaybe)
import Data.Ord (Down(Down))
import Debug.Trace (traceShow, traceShowId)
import Text.Megaparsec (MonadParsec, parseMaybe, sepEndBy)
import Text.Megaparsec.Char (newline, string, space)
import Text.Megaparsec.Char.Lexer (decimal, signed)

data Bot a = Bot {x :: a, y :: a, z :: a, r :: a}

data Volume a = Volume {x0 :: a, x1 :: a, y0 :: a, y1 :: a, z0 :: a, z1 :: a}
  deriving (Eq, Ord, Show)

parser :: (Integral a, MonadParsec e String m) => m [Bot a]
parser = flip sepEndBy newline $ Bot
    <$> (string "pos=<" *> signed space decimal)
    <*> (string "," *> signed space decimal)
    <*> (string "," *> signed space decimal)
    <*> (string ">, r=" *> decimal)

measure :: (Num a) => Bot a -> (a, Volume a)
measure Bot {..} = (boxSize v, v)
  where v = Volume
          { x0 = -x + y + z - r, y0 = x - y + z - r, z0 = x + y - z - r
          , x1 = -x + y + z + r, y1 = x - y + z + r, z1 = x + y - z + r
          }

(*?) :: (Ord a) => Volume a -> Volume a -> Maybe (Volume a)
Volume ax0 ax1 ay0 ay1 az0 az1 *? Volume bx0 bx1 by0 by1 bz0 bz1
  | x0 <= x1, y0 <= y1, z0 <= z1 = Just Volume {..}
  | otherwise = Nothing
  where x0 = max ax0 bx0; y0 = max ay0 by0; z0 = max az0 bz0
        x1 = min ax1 bx1; y1 = min ay1 by1; z1 = min az1 bz1

boxSize :: (Num a) => Volume a -> a
boxSize Volume {..} = (x1 - x0 + 1) * (y1 - y0 + 1) * (z1 - z0 + 1)

boxToOrigin :: (Integral a, Ord a) => Volume a -> a
boxToOrigin Volume {..} = minimum
  [ abs x + abs y + abs z
  | x <- [(y0 + z0) `div` 2..(y1 + z1) `div` 2]
  , y <- [(x0 + z0) `div` 2..(x1 + z1) `div` 2]
  , z <- [(x0 + y0) `div` 2..(x1 + y1) `div` 2]
  , x0 <= y + z - x, y + z - x <= x1
  , y0 <= x + z - y, x + z - y <= y1
  , z0 <= x + y - z, x + y - z <= z1
  ]

maximumsOn :: (Ord b) => (a -> b) -> [a] -> [a]
maximumsOn f = map fst . foldl maxGroup [] . (zip `ap` map f)
  where maxGroup as@((_, y):_) a@(_, x) = case compare x y of LT -> as; EQ -> a:as; GT -> [a]
        maxGroup _ a = [a]

day23a :: String -> Maybe Int
day23a input = do
    bots <- parseMaybe @() (parser @Int) input
    listToMaybe $ sortOn Down
      [ length $ filter test bots
      | Bot {..} <- maximumsOn r bots
      , let test Bot {x = x', y = y', z = z'} = abs (x' - x) + abs (y' - y) + abs (z' - z) <= r
      ]

day23b :: String -> Maybe Int
day23b input = do
    bots <- parseMaybe @() (parser @Int) input
    let start = Map.fromListWith IntSet.union $
            zip (measure <$> bots) (IntSet.singleton <$> [1..])
    return $ snd $ execState (go start) (0, 0)

go :: (MonadState (Int, a) m, Integral a, Ord a, Show a) => Map (a, Volume a) IntSet -> m ()
go (Map.minViewWithKey -> Just (((_, v), n), vs)) = record n' v >> go vs' >> go vs
  where (now, later) = Map.partitionWithKey (\w _ -> v == w) $
            Map.mapKeysMonotonic fromJust $ Map.delete Nothing $
            Map.mapKeysWith IntSet.union (\(_, w) -> v *? w) vs
        n' = IntSet.unions $ n : Map.elems now
        vs' = Map.mapKeys (\w -> (boxSize w, w)) $ IntSet.union n' <$> later
go _ = return ()

record :: (MonadState (Int, a) m, Integral a, Ord a, Show a) => IntSet -> Volume a -> m ()
record n v = do
    (m, w) <- get
    when (IntSet.size n > m || IntSet.size n == m && boxToOrigin v < w) $
        traceShow (n, v) $ put $ traceShowId (IntSet.size n, boxToOrigin v)
