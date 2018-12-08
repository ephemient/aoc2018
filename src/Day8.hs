{-|
Module:         Day8
Description:    <https://adventofcode.com/2018/day/8 Day 8: Memory Maneuver>
-}
{-# LANGUAGE FlexibleContexts, LambdaCase, TypeApplications #-}
module Day8 (day8a, day8b) where

import Data.Function (on)
import Data.Maybe (fromMaybe)
import Text.Megaparsec (MonadParsec, count, parseMaybe)
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Char.Lexer (decimal)

parser :: (MonadParsec e String m, Integral i) => ([i] -> [i] -> i) -> m i
parser f = parser' <* space
  where parser' = do
            n <- space *> decimal
            m <- space *> decimal
            f <$> count n parser' <*> count m (space *> decimal)

day8a :: String -> Maybe Int
day8a = parseMaybe $ parser @() $ on (+) sum

day8b :: String -> Maybe Int
day8b = parseMaybe $ parser @() $ \case
    [] -> sum
    xs -> sum . map (fromMaybe 0 . flip lookup (zip [1..] xs))
