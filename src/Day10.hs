{-|
Module:         Day10
Description:    <https://adventofcode.com/2018/day/10 Day 10: The Stars Align>
-}
{-# LANGUAGE FlexibleContexts, NamedFieldPuns, RecordWildCards, TypeApplications #-}
module Day10 (day10) where

import Control.Arrow ((&&&))
import Control.Monad (ap)
import Data.Array (accumArray, elems)
import Data.Bool (bool)
import Data.Foldable (toList)
import Data.Ix (Ix, rangeSize)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.List.Split (chunksOf)
import Data.Maybe (listToMaybe)
import Data.Semigroup (Max(Max), Min(Min), sconcat)
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

step :: (Num a) => Point a -> Point a
step point@Point {..} = point {px = px + px', py = py + py'}

bbox :: (Num a, Ord a) => NonEmpty (Point a) -> ((a, a), (a, a))
bbox points = ((minY, minX), (maxY, maxX))
  where (Min minY, Min minX, Max maxY, Max maxX) = sconcat $ project <$> points
        project Point {px, py} = (Min py, Min px, Max py, Max px)

display :: (Foldable t, Ix a) => ((a, a), (a, a)) -> t (Point a) -> String
display b@((_, minX), (_, maxX)) = unlines . chunksOf (rangeSize (minX, maxX)) .
    map (bool '░' '▓') . elems .  accumArray (||) False b . map ((py &&& px) &&& pure True) . toList

day10 :: String -> Maybe (Int, String)
day10 input = parseMaybe @() (parser @Int) input >>= nonEmpty >>= \points -> listToMaybe
  [ (i, display b result)
  | (i, (result, b), (_, b')) <- zip3 [0..] `ap` tail $ (,) `ap` bbox <$> iterate (fmap step) points
  , rangeSize b < rangeSize b'
  ]
