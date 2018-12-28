{-|
Module:         Day10
Description:    <https://adventofcode.com/2018/day/10 Day 10: The Stars Align>
-}
{-# LANGUAGE FlexibleContexts, RecordWildCards, TupleSections, TypeApplications #-}
module Day10 (day10) where

import Data.Bool (bool)
import Data.IntMap (assocs, fromListWith)
import Data.List (sortOn, tails)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (listToMaybe)
import Data.Ord (Down(Down))
import Data.Semigroup (Max(Max), Min(Min), sconcat)
import Data.Set (elems, fromList, member)
import Text.Megaparsec (MonadParsec, between, parseMaybe, sepEndBy)
import Text.Megaparsec.Char (char, newline, space, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)

data Point a = Point {px :: !a, py :: !a, px' :: !a, py' :: !a}

parser :: (Integral a, MonadParsec e String m) => m [Point a]
parser = flip sepEndBy newline $ between (string "position=<") (char '>') $ Point
    <$> (space *> signed space decimal)
    <*> (char ',' *> space *> signed space decimal)
    <*> (string "> velocity=<" *> space *> signed space decimal)
    <*> (char ',' *> space *> signed space decimal)

day10 :: String -> Maybe (Int, String)
day10 input = do
    points <- parseMaybe @() (parser @Int) input
    (t, _) <- listToMaybe $ sortOn (Down @Int . snd) $ assocs $ fromListWith (+) $ (, 1) <$> do
        Point {px = x1, py = y1, px' = dx1, py' = dy1}:rest <- tails points
        Point {px = x2, py = y2, px' = dx2, py' = dy2} <- rest
        [t | dx1 /= dx2, (t, 0) <- [(x1 - x2) `divMod` (dx2 - dx1)]] ++
            [t | dy1 /= dy2, (t, 0) <- [(y1 - y2) `divMod` (dy2 - dy1)]]
    let results = fromList [(px + px' * t, py + py' * t) | Point {..} <- points]
        minMaxXY (x, y) = (Min x, Min y, Max x, Max y)
    (Min minX, Min minY, Max maxX, Max maxY) <- sconcat <$> nonEmpty (minMaxXY <$> elems results)
    return $ (t,) $ unlines
        [[bool '░' '▓' $ (x, y) `member` results | x <- [minX..maxX]] | y <- [minY..maxY]]
