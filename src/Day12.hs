{-|
Module:         Day12
Description:    <https://adventofcode.com/2018/day/12 Day 12: Subterranean Sustainability>
-}
{-# LANGUAGE FlexibleContexts, TypeApplications, ViewPatterns #-}
module Day12 (day12) where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet (findMax, findMin, foldl', fromList, member, null)
import Data.List.Split (divvy)
import Data.Set (Set)
import qualified Data.Set as Set (fromList, member)
import Text.Megaparsec (MonadParsec, between, count, parseMaybe, sepEndBy, some)
import Text.Megaparsec.Char (newline, oneOf, string)

parser :: (MonadParsec e String m) => m (IntSet, Set [Bool])
parser = do
    initial <- between (string "initial state: ") newline (some spot)
    mappings <- map fst . filter snd <$> (newline *> sepEndBy line newline)
    return (IntSet.fromList . map fst . filter snd . zip [0..] $ initial, Set.fromList mappings)
  where line = (,) <$> count 5 spot <*> (string " => " *> spot)
        spot = (== '#') <$> oneOf "#."

day12 :: Int -> String -> Int
day12 n (parseMaybe @() parser -> Just (initial, mappings))
  | n >= 0, not $ IntSet.null initial, all or mappings
  = IntSet.foldl' (+) 0 $ iterate step initial !! n
  where step spots = let (lo, hi) = (IntSet.findMin spots, IntSet.findMax spots) in
            IntSet.fromList $ map fst $ filter snd $ zip [lo - 2..hi + 2] $
            map (`Set.member` mappings) $ divvy 5 1 $ False : False : False : False :
            map (`IntSet.member` spots) [lo..hi] ++ [False, False, False, False]
day12 _ _ = error "?!"
