{-|
Module:         Day12
Description:    <https://adventofcode.com/2018/day/12 Day 12: Subterranean Sustainability>
-}
{-# LANGUAGE FlexibleContexts, LambdaCase, TypeApplications, ViewPatterns #-}
module Day12 (day12) where

import Control.Monad.State (evalState, gets, modify)
import Data.List (dropWhileEnd)
import Data.List.Split (divvy)
import qualified Data.Map as Map (empty, insert, lookup)
import Data.Set (Set)
import qualified Data.Set as Set (fromList, member)
import Text.Megaparsec (MonadParsec, between, count, parseMaybe, sepEndBy, some)
import Text.Megaparsec.Char (newline, oneOf, string)

parser :: (MonadParsec e String m) => m ([Bool], Set [Bool])
parser = (,) <$> between (string "initial state: ") newline (some spot)
    <*> (Set.fromList . map fst . filter snd <$> (newline *> sepEndBy line newline))
  where line = (,) <$> count 5 spot <*> (string " => " *> spot)
        spot = (== '#') <$> oneOf "#."

day12 :: Int -> String -> Maybe Int
day12 n (parseMaybe @() parser -> Just (initial, mappings)) | n >= 0, not $ null initial =
    Just $ flip evalState Map.empty $ loop 0 0 initial
  where loop i offset state | i == n = return $ sum $ map fst $ filter snd $ zip [offset..] state
        loop i offset state = do
            modify (Map.insert state (i, offset))
            let (i1, (offset1, state')) = (i + 1, step offset state)
            gets (Map.lookup state') >>= \case
                Just (i0, offset0) -> loop (n - (n - i1) `mod` (i1 - i0))
                    (offset1 + (n - i1) `div` (i1 - i0) * (offset1 - offset0)) state'
                Nothing -> loop i1 offset1 state'
        step offset state = (offset + add - 2, state'')
          where extended = False : False : False : False : state ++ [False, False, False, False]
                state' = dropWhileEnd not $ map (`Set.member` mappings) $ divvy 5 1 extended
                (length -> add, state'') = span not state'
day12 _ _ = Nothing
