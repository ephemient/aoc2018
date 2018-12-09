{-|
Module:         Day9
Description:    <https://adventofcode.com/2018/day/9 Day 9: Marble Mania>
-}
{-# LANGUAGE FlexibleContexts, RecordWildCards, TypeApplications #-}
module Day9 (day9a, day9b, play) where

import Control.Arrow (second)
import Data.IntMap (IntMap, elems, empty, insertWith)
import Data.List (foldl')
import Data.Sequence (Seq, deleteAt, index, insertAt, length, singleton)
import Prelude hiding (length)
import Text.Megaparsec (MonadParsec, parseMaybe, skipManyTill, takeRest)
import Text.Megaparsec.Char (satisfy)
import Text.Megaparsec.Char.Lexer (decimal)

data Game a = Game {scores :: IntMap a, pos :: Int, ring :: Seq a}

parser :: (MonadParsec e String m, Integral players, Integral target) => m (players, target)
parser = (,) <$> decimal <*> skipManyTill (satisfy $ const True) decimal <* takeRest

play :: Int -> Int -> Int
play players target = maximum . elems . scores $
    foldl' acc Game {scores = empty, pos = 0, ring = singleton 0} [1..target]
  where acc game@Game {..} n
          | n `mod` 23 == 0
          = let scores' = insertWith (+) (n `mod` players) (n + index ring pos') scores
                pos' = (pos - 7) `mod` length ring
                ring' = deleteAt pos' ring
            in game {scores = scores', pos = pos', ring = ring'}
          | otherwise
          = let pos' = (pos + 1) `mod` length ring + 1
            in game {pos = pos', ring = insertAt pos' n ring}

day9a :: String -> Maybe Int
day9a = fmap (uncurry play) . parseMaybe @() parser

day9b :: String -> Maybe Int
day9b = fmap (uncurry play . second (* 100)) . parseMaybe @() parser
