{-|
Module:         Day6
Description:    <https://adventofcode.com/2018/day/6 Day 6: Chronal Coordinates>
-}
{-# LANGUAGE FlexibleContexts, LambdaCase, TupleSections, TypeApplications, ViewPatterns #-}
module Day6 (day6a, day6b) where

import Control.Arrow ((&&&), (***))
import Control.Monad (foldM, forM)
import Data.Array.IArray (assocs)
import Data.Array.ST (inRange, newArray, readArray, runSTArray, writeArray)
import Data.Either (partitionEithers)
import Data.List.NonEmpty (nonEmpty)
import Data.Map.Strict (elems, fromListWith)
import Data.Maybe (catMaybes)
import Data.Semigroup (Max(Max, getMax), Min(Min, getMin), sconcat)
import Data.Set (empty, fromList, insert, notMember, singleton)

parse :: String -> [(Int, Int)]
parse = map parse1 . lines
  where parse1 s = read $ "(" ++ s ++ ")"

day6a :: String -> Maybe Int
day6a (parse -> input@(nonEmpty -> Just input')) = Just $ postprocess $ runSTArray $ do
    a <- newArray b Nothing
    let loop _ [] = return ()
        loop d queues = loop' d queues >>= loop (d + 1)
        loop' d queues = fmap catMaybes $ forM queues $ \(n, q) -> do
            next <- foldM (spread n d) empty q
            return $ if null next then Nothing else Just (n, next)
        spread n d q p = readArray a p >>= \case
            Just (_, d') | d' < d -> return q
            (fmap snd -> d') -> foldr insert q (neighbors p) <$
                writeArray a p (Just (if d' == Just d then Nothing else Just n, d))
    loop (0 :: Int) $ zip @Int [0..] $ map singleton input
    return a
  where b@((x0, y0), (x1, y1)) = ((getMin *** getMin) *** (getMax *** getMax)) . sconcat $
            ((Min *** Min) &&& (Max *** Max)) <$> input'
        neighbors (x, y) = filter (inRange b) [(x - 1, y), (x, y + 1), (x + 1, y), (x, y - 1)]
        postprocess a = maximum . elems . fromListWith (+) $
            (, 1) <$> filter (`notMember` exclude) include
          where (include, fromList -> exclude) = partitionEithers
                  [ (if x == x0 || x == x1 || y == y0 || y == y1 then Right else Left) n
                  | ((x, y), Just (Just n, _)) <- assocs a
                  ]
day6a _ = Nothing

day6b :: Int -> String -> Maybe Int
day6b limit (parse -> input@(unzip -> (xs@(nonEmpty -> Just xs'), ys@(nonEmpty -> Just ys')))) =
    Just $ length $ filter (< limit) [dx + dy | dx <- dxs', dy <- dys']
  where radius = limit `div` length input
        (Min x0, Max x1) = sconcat $ (Min &&& Max) <$> xs'
        (Min y0, Max y1) = sconcat $ (Min &&& Max) <$> ys'
        dxs = [sum [abs $ x - x'| x' <- xs] | x <- [x0 - radius..x1 + radius]]
        dys = [sum [abs $ y - y'| y' <- ys] | y <- [y0 - radius..y1 + radius]]
        dxs' = filter (< limit - minimum dys) dxs
        dys' = filter (< limit - minimum dxs) dys
day6b _ _ = Nothing
