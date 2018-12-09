{-|
Module:         Day9
Description:    <https://adventofcode.com/2018/day/9 Day 9: Marble Mania>
-}
{-# LANGUAGE FlexibleContexts, Strict, TypeApplications #-}
module Day9 (day9a, day9b, play) where

import Control.Arrow (second)
import Control.Monad (foldM, forM_)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Data.IntMap (elems, empty, insertWith)
import Text.Megaparsec (MonadParsec, parseMaybe, skipManyTill, takeRest)
import Text.Megaparsec.Char (satisfy)
import Text.Megaparsec.Char.Lexer (decimal)

parser :: (MonadParsec e String m, Integral players, Integral target) => m (players, target)
parser = (,) <$> decimal <*> skipManyTill (satisfy $ const True) decimal <* takeRest

play :: Int -> Int -> Int
play players target = maximum . elems $ runST $ do
    a <- newArray (0, target - 1) 0 :: ST s (STUArray s Int Int)
    let ix pos i = (pos + i) `mod` target
        acc (pos, size, scores) n
          | n `mod` 23 == 0 = do
                forM_ [1..6] $ \i -> readArray a (ix pos $ size - i) >>= writeArray a (ix pos (-i))
                m <- readArray a $ ix pos $ size - 7
                let scores' = insertWith (+) (n `mod` players) (n + m) scores
                return (ix pos (-6), size - 1, scores')
          | otherwise = do
                readArray a pos >>= writeArray a (ix pos size)
                readArray a (ix pos 1) >>= writeArray a (ix pos $ size + 1)
                writeArray a (ix pos 1) n
                return (ix pos 1, size + 1, scores)
    (_, _, scores) <- foldM acc (0, 1, empty) [1..target]
    return scores

day9a :: String -> Maybe Int
day9a = fmap (uncurry play) . parseMaybe @() parser

day9b :: String -> Maybe Int
day9b = fmap (uncurry play . second (* 100)) . parseMaybe @() parser
