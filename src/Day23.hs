{-|
Module:         Day23
Description:    <https://adventofcode.com/2018/day/23 Day 23: Experimental Emergency Teleportation>
-}
{-# LANGUAGE FlexibleContexts, LambdaCase, RecordWildCards, TypeApplications, ViewPatterns #-}
module Day23 (day23a, day23b) where

import Control.Monad (ap)
import Control.Monad.State.Strict (execState, get, gets, put)
import Data.Function (on)
import Data.IntSet (singleton, size, union, unions)
import Data.List (foldl', sortOn)
import Data.Map.Strict (elems, delete, fromListWith, mapKeysMonotonic, mapKeysWith, maxViewWithKey, partitionWithKey)
import Data.Maybe (fromJust, listToMaybe)
import Data.Ord (Down(Down))
import Text.Megaparsec (MonadParsec, parseMaybe, sepEndBy)
import Text.Megaparsec.Char (newline, string, space)
import Text.Megaparsec.Char.Lexer (decimal, signed)

data Bot a = Bot {x :: a, y :: a, z :: a, r :: a}

data Quad a = Quad {xyz :: a, xy'z :: a, x'y'z :: a, x'yz :: a}
  deriving (Eq, Ord)

data Octa a = Octa {lower :: Quad a, upper :: Quad a}
  deriving (Eq)

instance (Ord a) => Ord (Octa a) where
    compare = (compare `on` lower) <> (flip compare `on` upper)

parser :: (Integral a, MonadParsec e String m) => m [Bot a]
parser = flip sepEndBy newline $ Bot
    <$> (string "pos=<" *> signed space decimal)
    <*> (string "," *> signed space decimal)
    <*> (string "," *> signed space decimal)
    <*> (string ">, r=" *> decimal)

maximumsOn :: (Ord b) => (a -> b) -> [a] -> [a]
maximumsOn f = map fst . foldl maxGroup [] . (zip `ap` map f)
  where maxGroup as@((_, y):_) a@(_, x) = case compare x y of LT -> as; EQ -> a:as; GT -> [a]
        maxGroup _ a = [a]

botToOct :: (Num a) => Bot a -> Octa a
botToOct Bot {..} = Octa {..}
  where t = x + y + z; u = x + y - z; v = x - y - z; w = x - y + z
        lower = Quad (t - r) (u - r) (v - r) (w - r)
        upper = Quad (t + r) (u + r) (v + r) (w + r)

intersect :: (Num a, Ord a) => Octa a -> Octa a -> Maybe (Octa a)
intersect (Octa q1 q2) (Octa q3 q4)
  | p <= t, q <= u, r <= v, s <= w = Just $ Octa (Quad p q r s) (Quad t u v w)
  | otherwise = Nothing
  where p = (max `on` xyz) q1 q3; q = (max `on` xy'z) q1 q3
        r = (max `on` x'y'z) q1 q3; s = (max `on` x'yz) q1 q3
        t = (min `on` xyz) q2 q4; u = (min `on` xy'z) q2 q4
        v = (min `on` x'y'z) q2 q4; w = (min `on` x'yz) q2 q4

octaToOrigin :: (Num a, Ord a) => Octa a -> a
octaToOrigin (Octa (Quad p q r s) (Quad t u v w)) = foldl' max 0 $
    [min (abs p) (abs t) | signum p * signum t >= 0] ++
    [min (abs q) (abs u) | signum q * signum u >= 0] ++
    [min (abs r) (abs v) | signum r * signum v >= 0] ++
    [min (abs s) (abs w) | signum s * signum w >= 0]

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
    let octs = fromListWith union $ zip (botToOct <$> bots) (singleton <$> [1..])
    snd $ execState (go octs) (0, Nothing)
  where go octs@(size . unions . elems -> remaining) = gets fst >>= \case
            best | best <= remaining, Just ((oct, n), rest) <- maxViewWithKey octs -> do
                let (super, sub) = partitionWithKey (\oct' _ -> oct == oct') $
                        mapKeysMonotonic fromJust $ delete Nothing $
                        mapKeysWith union (intersect oct) rest
                    n' = unions $ n : elems super
                record (size n') $ octaToOrigin oct
                go $ union n' <$> sub
                go rest
            _ -> return ()
        record n m = get >>= \case
            (best, _) | best < n -> put (n, Just m)
            (best, m') | best == n -> put (n, min m' $ Just m)
            _ -> return ()
