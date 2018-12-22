{-|
Module:         Day22
Description:    <https://adventofcode.com/2018/day/22 Day 22: Mode Maze>
-}
{-# LANGUAGE FlexibleContexts, LambdaCase, MultiWayIf, TupleSections, TypeApplications, ViewPatterns #-}
module Day22 (day22a, day22b) where

import Control.Monad.ST (runST)
import Data.Array.Unboxed (IArray, Ix, UArray, (!), (//), accumArray, assocs, bounds, elems, listArray, rangeSize)
import Data.Function (on)
import qualified Data.Heap as Heap (FstMinPolicy, insert, singleton, view)
import qualified Data.Map.Strict as Map (empty, insert, lookup)
import Data.STRef (newSTRef, modifySTRef', readSTRef, writeSTRef)
import Data.List ((\\), scanl')
import Text.Megaparsec (MonadParsec, between, parseMaybe)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

parser :: (Integral a, Integral i, MonadParsec e String m) => m (a, (i, i))
parser = (,) <$> between (string "depth: ") newline decimal
    <*> between (string "target: ") newline ((,) <$> decimal <*> (char ',' *> decimal))

erosion, risk :: (Integral a) => a -> a -> a
erosion depth = (`mod` 20183) . (+ depth)
risk depth = (`mod` 3) . erosion depth

makeMaze :: (IArray a e, Enum e, Integral e, Ix e) => e -> (e, e) -> (e, e) -> a (e, e) e
makeMaze depth (targetX, targetY) size@(_, sizeY) =
    listArray ((0, 0), size) $ scanl acc [0, 48271..] [1..] >>= take (rangeSize (0, sizeY))
  where acc (drop 1 -> prev) x = scanl' (acc' x) (16807 * x) $ zip [1..] prev
        acc' x left (y, up)
          | x == targetX && y == targetY = 0
          | otherwise = erosion depth left * erosion depth up

growMaze :: (IArray a e, Enum e, Integral e, Ix e) => e -> (e, e) -> a (e, e) e -> a (e, e) e
growMaze depth (sizeX, sizeY) prior = top // bottom
  where ((0, 0), (priorX@(max sizeX -> sizeX'), priorY@(max sizeY -> sizeY'))) = bounds prior
        top = accumArray (flip const) 0 ((0, 0), (sizeX', sizeY')) $ assocs prior ++ concat right
        right = scanl accR [((x, 0), 16807 * x) | x <- [priorX + 1..sizeX']] rightCol
        rightCol = [(y, prior ! (priorX, y)) | y <- [1..priorY]]
        accR prev (y, v) = zip ((, y) <$> [priorX + 1..]) $ drop 1 $ scanl' (-*-) v $ snd <$> prev
        bottom = concat $ drop 1 $ scanl accD bottomRow [priorY + 1..sizeY']
        bottomRow = [(p, top ! p) | x <- [0..sizeX'], let p = (x, priorY)]
        accD prev y = zip ((, y) <$> [0..]) $ scanl' (-*-) (48271 * y) $ snd <$> drop 1 prev
        (-*-) = (*) `on` erosion depth

day22a :: String -> Maybe Int
day22a input = do
    (depth, target) <- parseMaybe @() (parser @Int @Int) input
    return $ sum $ risk depth <$> elems (makeMaze @UArray depth target target)

day22b :: String -> Maybe Int
day22b input = do
    (depth, target) <- parseMaybe @() (parser @Int @Int) input
    runST $ do
        mazeRef <- newSTRef $ makeMaze @UArray depth target target
        wsRef <- newSTRef Map.empty
        let bfs (Heap.view -> Just ((_, (w, k@(e, p))), heap))
              | 1 <- e, (0, 0) <- p = return $ Just w
              | otherwise = Map.lookup k <$> readSTRef wsRef >>= \case
                    Just w' | w' <= w -> bfs heap
                    _ -> modifySTRef' wsRef (Map.insert k w) >>
                        foldr (Heap.insert . estimate) heap <$> neighbors w e p >>= bfs
            bfs _ = return Nothing
            neighbors w e p@(x, y) = do
                maze <- readSTRef mazeRef
                let ((0, 0), (maxX, maxY)) = bounds maze
                maze' <- if x < maxX && y < maxY then return maze else do
                    let maze' = growMaze depth (grow x maxX, grow y maxY) maze
                    maze' <$ writeSTRef mazeRef maze'
                ws <- readSTRef wsRef
                return $ filter (\(w', k) -> maybe True (> w') $ Map.lookup k ws) $
                    [(w + 7, (e', p)) | e' <- [0..2] \\ [risk depth $ maze' ! p, e]] ++
                    [(w + 1, (e, p')) | x > 0, let p' = (x - 1, y), risk depth (maze' ! p') /= e] ++
                    [(w + 1, (e, p')) | y > 0, let p' = (x, y - 1), risk depth (maze' ! p') /= e] ++
                    [(w + 1, (e, p')) | let p' = (x + 1, y), risk depth (maze' ! p') /= e] ++
                    [(w + 1, (e, p')) | let p' = (x, y + 1), risk depth (maze' ! p') /= e]
        bfs $ Heap.singleton @Heap.FstMinPolicy (0, (0, (1, target)))
  where estimate a@(w, (e, (x, y))) = (w + x + y + if e == 1 then 0 else 7, a)
        grow n = head . dropWhile (<= n) . iterate (* 2)
