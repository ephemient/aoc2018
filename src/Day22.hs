{-|
Module:         Day22
Description:    <https://adventofcode.com/2018/day/22 Day 22: Mode Maze>
-}
{-# LANGUAGE FlexibleContexts, LambdaCase, MultiWayIf, TypeApplications, ViewPatterns #-}
module Day22 (day22a, day22b) where

import Control.Monad (filterM)
import Control.Monad.ST (ST, runST)
import Data.Array.Unboxed (IArray, Ix, UArray, assocs, elems, listArray, rangeSize)
import Data.Function (on)
import qualified Data.Heap as Heap (FstMinPolicy, insert, singleton, view)
import qualified Data.Map.Strict as Map (empty, fromAscList, insert, lookup)
import Data.STRef (newSTRef, modifySTRef', readSTRef)
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

makeMaze' :: Int -> (Int, Int) -> ST s ((Int, Int) -> ST s Int)
makeMaze' depth target = do
    cache <- newSTRef $ Map.fromAscList $ assocs $ makeMaze @UArray depth target target
    let get p = Map.lookup p <$> readSTRef cache >>= \case
            Just v -> return v
            _ | (x, y) <- p -> do
                v <- if
                  | 0 <- y -> return $ x * 16807
                  | 0 <- x -> return $ y * 48271
                  | otherwise -> ((*) `on` erosion depth) <$> get (x - 1, y) <*> get (x, y - 1)
                v <$ modifySTRef' cache (Map.insert p v)
    return get

day22a :: String -> Maybe Int
day22a input = do
    (depth, target) <- parseMaybe @() (parser @Int @Int) input
    return $ sum $ risk depth <$> elems (makeMaze @UArray depth target target)

day22b :: String -> Maybe Int
day22b input = do
    (depth, target) <- parseMaybe @() (parser @Int @Int) input
    runST $ makeMaze' depth target >>= \f -> do
        ws <- newSTRef Map.empty
        let bfs (Heap.view -> Just ((_, (w, k@(e, p))), heap))
              | 1 <- e, (0, 0) <- p = return $ Just w
              | otherwise = Map.lookup k <$> readSTRef ws >>= \case
                    Just w' | w' <= w -> bfs heap
                    _ -> modifySTRef' ws (Map.insert k w) >>
                        foldr (Heap.insert . estimate) heap <$> neighbors w e p >>= bfs
            bfs _ = return Nothing
            neighbors w e p@(x, y) = do
                r <- risk depth <$> f p
                let change = [(w + 7, (e', p)) | e' <- [0..2] \\ [r, e]]
                move <- filterM (\(_, (e', p')) -> (/= e') . risk depth <$> f p') $
                    [(w + 1, (e, (x - 1, y))) | x > 0] ++
                    [(w + 1, (e, (x, y - 1))) | y > 0] ++
                    [(w + 1, (e, (x + 1, y))), (w + 1, (e, (x, y + 1)))]
                return $ change ++ move
            estimate a@(w, (_, (x, y))) = (w + x + y, a)
        bfs $ Heap.singleton @Heap.FstMinPolicy (0, (0, (1, target)))
