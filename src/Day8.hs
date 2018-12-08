{-|
Module:         Day8
Description:    <https://adventofcode.com/2018/day/8 Day 8: Memory Maneuver>
-}
{-# LANGUAGE FlexibleContexts, LambdaCase, TypeApplications #-}
module Day8 (day8a, day8b) where

import Data.Function (on)
import Text.Megaparsec (MonadParsec, count, parseMaybe)
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Char.Lexer (decimal)

parser :: (MonadParsec e String m, Integral i) => ([i] -> [i] -> i) -> m i
parser f = parser'
  where parser' = do
            n <- decimal <* space
            m <- decimal <* space
            f <$> count n parser' <*> count m (decimal <* space)

day8a :: String -> Maybe Int
day8a = parseMaybe $ parser @() $ on (+) sum

day8b :: String -> Maybe Int
day8b = parseMaybe $ parser @() $ \case
    [] -> sum
    xs -> sum . map ((0 : xs ++ repeat 0) !!)
