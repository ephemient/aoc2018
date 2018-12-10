{-|
Module:         Day10
Description:    <https://adventofcode.com/2018/day/10 Day 10: The Stars Align>
-}
{-# LANGUAGE FlexibleContexts, GADTs, NamedFieldPuns, RecordWildCards, TypeApplications #-}
module Day10 (day10) where

import Control.Monad (ap)
import Data.Array (accumArray, elems)
import Data.Bool (bool)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.List.Split (chunksOf)
import Data.Maybe (listToMaybe)
import Data.Semigroup (Max(Max), Min(Min), sconcat)
import Text.Megaparsec (MonadParsec, between, parseMaybe, sepEndBy)
import Text.Megaparsec.Char (char, newline, space, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)

data Point a = Point {px :: a, py :: a, px' :: a, py' :: a}

parser :: (MonadParsec e String m, Integral a) => m [Point a]
parser = flip sepEndBy newline $ between (string "position=<") (char '>') $ Point
    <$> (space *> signed space decimal)
    <*> (char ',' *> space *> signed space decimal)
    <*> (string "> velocity=<" *> space *> signed space decimal)
    <*> (char ',' *> space *> signed space decimal)

step :: (Num a) => Point a -> Point a
step point@Point {..} = point {px = px + px', py = py + py'}

minMaxXY :: (Num a, Ord a) => NonEmpty (Point a) -> (a, a, a, a)
minMaxXY points = (minX, minY, maxX, maxY)
  where (Min minX, Min minY, Max maxX, Max maxY) = sconcat $ project <$> points
        project Point {px, py} = (Min px, Min py, Max px, Max py)

display :: (Foldable t) => (Int, Int, Int, Int) -> t (Point Int) -> String
display (minX, minY, maxX, maxY) =
    unlines . chunksOf (maxX - minX + 1) . map (bool '░' '▓') . elems .
    accumArray (||) False ((minY, minX), (maxY, maxX)) . map project . toList
  where project Point {px, py} = ((py, px), True)

day10 :: String -> Maybe (Int, String)
day10 input = do
    points <- nonEmpty =<< parseMaybe @() parser input
    listToMaybe
      [ (i, display b result)
      | (i, (result, b@(minX, minY, maxX, maxY)), (_, (minX', minY', maxX', maxY'))) <-
            zip3 [0..] `ap` tail $ (,) `ap` minMaxXY <$> iterate (fmap step) points
      , (maxX - minX + 1) * (maxY - minY + 1) < (maxX' - minX' + 1) * (maxY' - minY' + 1)
      ]
