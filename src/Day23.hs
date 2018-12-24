{-|
Module:         Day23
Description:    <https://adventofcode.com/2018/day/23 Day 23: Experimental Emergency Teleportation>
-}
{-# LANGUAGE FlexibleContexts, RecordWildCards, TypeApplications #-}
module Day23 (day23a, day23b) where

import Control.Monad (ap)
import Data.List (foldl', sortOn)
import Data.Maybe (listToMaybe)
import Data.Ord (Down(Down))
import Debug.Trace (traceShow, traceShowId)
import Text.Megaparsec (MonadParsec, parseMaybe, sepEndBy)
import Text.Megaparsec.Char (newline, string, space)
import Text.Megaparsec.Char.Lexer (decimal, signed)

data Bot a = Bot {x :: a, y :: a, z :: a, r :: a}

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
    boxes <- map botToBox <$> parseMaybe @() (parser @Int) input
    let maxBoxes = map snd $ traceShowId $ maximumsOn fst
          [ (length overlaps, (lo, foldl' min4 hi overlaps))
          | (lo, hi) <- boxes
          , let overlaps = [hi' | (lo', hi') <- boxes, min4 lo lo' == lo', min4 lo hi' == lo]
          ]
    return $ minimum $ boxToOrigin <$> traceShowId maxBoxes
  where botToBox Bot {..} = ((t - r, u - r, v - r, w - r), (t + r, u + r, v + r, w + r))
          where t = x + y + z; u = x + y - z; v = x - y - z; w = x - y + z
        min4 (p, q, r, s) (t, u, v, w) = (min p t, min q u, min r v, min s w)
        boxToOrigin ((p, q, r, s), (t, u, v, w))
          | traceShow ((x0, y0, z0), (x1, y1, z1)) False = undefined
          | x0 > 0 && x1 > 0 && y0 > 0 && y1 > 0 && z0 > 0 && z1 > 0 = p
          | x0 > 0 && x1 > 0 && y0 > 0 && y1 > 0 && z0 < 0 && z1 < 0 = q
          | x0 > 0 && x1 > 0 && y0 < 0 && y1 < 0 && z0 < 0 && z1 < 0 = r
          | x0 > 0 && x1 > 0 && y0 < 0 && y1 < 0 && z0 > 0 && z1 > 0 = s
          | x0 < 0 && x1 < 0 && y0 < 0 && y1 < 0 && z0 < 0 && z1 < 0 = -t
          | x0 < 0 && x1 < 0 && y0 < 0 && y1 < 0 && z0 > 0 && z1 > 0 = -u
          | x0 < 0 && x1 < 0 && y0 > 0 && y1 > 0 && z0 > 0 && z1 > 0 = -v
          | x0 < 0 && x1 < 0 && y0 > 0 && y1 > 0 && z0 < 0 && z1 < 0 = -w
          | otherwise = error "unimplemented"
          where (x0, _) = (p + r) `divMod` 2; (x1, _) = (t + v) `divMod` 2
                (y0, _) = (q - r) `divMod` 2; (y1, _) = (u - v) `divMod` 2
                (z0, _) = (p - q) `divMod` 2; (z1, _) = (t - u) `divMod` 2
