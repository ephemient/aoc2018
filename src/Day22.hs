{-|
Module:         Day22
Description:    <https://adventofcode.com/2018/day/22 Day 22: Mode Maze>
-}
{-# LANGUAGE FlexibleContexts, MultiWayIf, TupleSections, TypeApplications, ViewPatterns #-}
module Day22 (day22a, day22b) where

import Control.Monad.State.Strict (evalState, get, put)
import Data.Array.Unboxed (IArray, Ix, UArray, (!), (//), accumArray, assocs, bounds, elems, listArray, rangeSize)
import Data.Function (on)
import qualified Data.Heap as Heap (MinPolicy, insert, singleton, view)
import qualified Data.Map.Strict as Map (empty, insert, lookup)
import Data.List (scanl')
import Text.Megaparsec (MonadParsec, between, parseMaybe)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

parser :: (Integral a, Integral i, MonadParsec e String m) => m (a, (i, i))
parser = (,) <$> between (string "depth: ") newline decimal
    <*> between (string "target: ") newline ((,) <$> decimal <*> (char ',' *> decimal))

erosion, risk :: (Integral a) => a -> a -> a
erosion depth = (`mod` 20183) . (+ depth)
risk depth = (`mod` 3) . erosion depth

makeMaze :: (IArray a e, Enum e, Integral e, Ix e) => e -> (e, e) -> a (e, e) e
makeMaze depth target@(_, targetY) = listArray ((0, 0), target)
    (scanl acc [0, 48271..] [1..] >>= take (rangeSize (0, targetY))) // [(target, 0)]
  where acc prev x = scanl' ((*) `on` erosion depth) (16807 * x) $ drop 1 prev

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
    return $ sum $ risk depth <$> elems (makeMaze @UArray depth target)

day22b :: String -> Maybe Int
day22b input = do
    (depth, target) <- parseMaybe @() (parser @Int @Int) input
    flip evalState (makeMaze @UArray depth target) $ do
        let bfs (Heap.view -> Just ((_, (w, k@(p, e))), heap), ws)
              | ((0, 0), 1) <- k = return $ Just w
              | Just w' <- Map.lookup k ws, w' <= Left w = bfs (heap, ws)
              | otherwise = neighbors w p e >>= bfs . commit heap (Map.insert k (Left w) ws)
            bfs _ = return Nothing
            neighbors w p@(x, y) e = do
                maze@(bounds -> ((0, 0), (maxX, maxY))) <- get
                maze' <- if x < maxX && y < maxY then return maze else
                    let maze' = growMaze depth (grow x maxX, grow y maxY) maze
                    in maze' <$ put maze'
                return $ do
                    x' <- [max x 1 - 1..x + 1]
                    y' <- if x == x' then [max y 1 - 1..y + 1] else [y]
                    let (p', r) = ((x', y'), risk depth $ maze' ! p')
                    if p == p' then [(w + 7, (p', 3 - r - e))] else [(w + 1, (p', e)) | r /= e]
            commit heap ws = foldr acc (heap, ws)
              where acc a@(w, k@(estimate w -> e)) (heap', ws')
                      | Just w' <- Map.lookup k ws, w' <= Right e = (heap', ws')
                      | otherwise = (Heap.insert (e, a) heap', Map.insert k (Right e) ws')
        bfs (Heap.singleton @Heap.MinPolicy (0, (0, (target, 1))), Map.empty)
  where estimate w ((x, y), e) = w + x + y + if e == 1 then 0 else 7
        grow n = head . dropWhile (<= n) . iterate (+ 50)
