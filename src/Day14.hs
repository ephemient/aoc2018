{-|
Module:         Day14
Description:    <https://adventofcode.com/2018/day/14 Day 14: Chocolate Charts>
-}
{-# LANGUAGE GADTs, NamedFieldPuns, RecordWildCards, TupleSections, TypeApplications, ViewPatterns #-}
module Day14 (day14a, day14b) where

import Control.Monad (zipWithM_)
import Control.Monad.Loops (andM, firstM, iterateUntilM)
import Control.Monad.ST (ST, runST)
import Data.Array.IArray (Ix, (!))
import Data.Array.ST (MArray, STUArray, getBounds, newArray_, readArray, runSTUArray, writeArray)
import Data.Char (digitToInt, intToDigit, isDigit)
import Data.List (genericLength)
import Data.Maybe (fromJust, isJust)
import Data.Word (Word8)

data Game a i e = Game {gamePos1 :: !i, gamePos2 :: !i, gameLength :: !i, gameScores :: !(a i e)}

game0 :: (MArray a e m, Ix i, Num i, Num e) => m (Game a i e)
game0 = do
    gameScores <- newArray_ (0, 1)
    writeArray gameScores 0 3
    writeArray gameScores 1 7
    return Game {gamePos1 = 0, gamePos2 = 1, gameLength = 2, gameScores}

step :: (MArray a e m, Integral i, Ix i, Integral e) => Game a i e -> m (Game a i e)
step Game {gamePos1 = p1, gamePos2 = p2, gameLength = n, gameScores = a} = do
    (0, top) <- getBounds a
    s1 <- readArray a p1
    s2 <- readArray a p2
    let append list = do
            let gamePos1 = (p1 + fromIntegral s1 + 1) `mod` gameLength
                gamePos2 = (p2 + fromIntegral s2 + 1) `mod` gameLength
                gameLength = n + genericLength list
                size:_ = filter (>= gameLength) $ iterate (* 2) $ top + 1
            gameScores <- if size <= top + 1 then return a else do
                a' <- newArray_ (0, size - 1)
                sequence_ [readArray a i >>= writeArray a' i | i <- [0..n - 1]]
                return a'
            Game {..} <$ zipWithM_ (writeArray gameScores) [n..] list
    append $ reverse $ case (s1 + s2) `divMod` 10 of (0, x) -> [x]; (x, y) -> [y, x]

day14a :: String -> String
day14a (read @Int . filter isDigit -> offset) = getDigits $ runSTUArray $
    gameScores <$> (game0 >>= iterateUntilM ((>= offset + 10) . gameLength) step)
  where getDigits a = [intToDigit $ fromIntegral @Word8 $ a ! i | i <- [offset..offset + 9]]

day14b :: String -> Int
day14b (map (fromIntegral . digitToInt) . filter isDigit -> digits) = runST $ do
    let nDigits = length digits
        step' (game, _) = do
            game'@Game {gameLength, gameScores} <- step game
            match <- firstM (matches gameScores) $ filter (>= 0)
                [gameLength - nDigits - 1, gameLength - nDigits]
            return (game', match)
        matches a n = andM [(== d) <$> readArray a i | (d, i) <- zip digits [n..]]
    game0' >>= fmap (fromJust . snd) . iterateUntilM (isJust . snd) step' . (, Nothing)
  where game0' = game0 :: ST s (Game (STUArray s) Int Word8)
