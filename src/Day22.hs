{-|
Module:         Day22
Description:    <https://adventofcode.com/2018/day/22 Day 22: Mode Maze>
-}
{-# LANGUAGE FlexibleContexts, LambdaCase, TupleSections, TypeApplications, ViewPatterns #-}
module Day22 (day22a, day22b) where

import Data.Array.ST (newArray, readArray, runSTArray, writeArray)
import Data.Array.Unboxed (IArray, Ix, UArray, (!), (//), elems, listArray, rangeSize)
import Data.Function (on)
import qualified Data.Heap as Heap (FstMinPolicy, insert, singleton, view)
import Data.List ((\\), scanl')
import Text.Megaparsec (MonadParsec, between, parseMaybe)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

parser :: (Integral a, Integral i, MonadParsec e String m) => m (a, (i, i))
parser = (,) <$> between (string "depth: ") newline decimal
    <*> between (string "target: ") newline ((,) <$> decimal <*> (char ',' *> decimal))

makeMaze :: (IArray a e, Integral e, Ix i, Num i) => e -> (i, i) -> (i, i) -> a (i, i) e
makeMaze depth target size@(_, rangeSize . (0,) -> n) = listArray ((0, 0), size)
    (map risk $ scanl acc [0, 48271..] [16807, 33614..] >>= take n) // [(target, risk 0)]
  where acc (drop 1 -> prev) start = scanl' ((*) `on` erosion) start prev
        erosion = (`mod` 20183) . (+ depth)
        risk = (`mod` 3) . erosion

day22a :: String -> Maybe Int
day22a input = do
    (depth, target) <- parseMaybe @() (parser @Int @Int) input
    return $ sum $ elems $ makeMaze @UArray depth target target

day22b :: String -> Maybe Int
day22b input = do
    (depth, target@(targetX, targetY)) <- parseMaybe @() (parser @Int @Int) input
    let size@(maxX, maxY) = (10 * targetX, 10 * targetY)
        maze = makeMaze @UArray depth target size
        bfs (Heap.view -> Just ((w, k@(e, p)), heap)) ws = readArray ws k >>= \case
            Just w' | w' <= w -> bfs heap ws
            _ -> writeArray ws k (Just w) >> if k == (1, target) then return ws else
                bfs (foldr Heap.insert heap $ neighbors w e p) ws
        bfs _ ws = return ws
        neighbors :: Int -> Int -> (Int, Int) -> [(Int, (Int, (Int, Int)))]
        neighbors w e p@(x, y) =
            [(w + 7, (e', p)) | e' <- [0..2] \\ [maze ! p, e]] ++
            [(w + 1, (e, p')) | x > 0, let p' = (x - 1, y), maze ! p' /= e] ++
            [(w + 1, (e, p')) | y > 0, let p' = (x, y - 1), maze ! p' /= e] ++
            [(w + 1, (e, p')) | y < maxY, let p' = (x, y + 1), maze ! p' /= e] ++
            [(w + 1, (e, p')) | x < maxX, let p' = (x + 1, y), maze ! p' /= e]
        distances = runSTArray $ newArray ((0, (0, 0)), (2, size)) Nothing >>=
            bfs (Heap.singleton @Heap.FstMinPolicy (0, (1, (0, 0))))
    distances ! (1, target)
