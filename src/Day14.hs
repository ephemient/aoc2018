{-|
Module:         Day14
Description:    <https://adventofcode.com/2018/day/14 Day 14: Chocolate Charts>
-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TypeApplications, ViewPatterns #-}
module Day14 (day14a, day14b) where

import Control.Monad (zipWithM_)
import qualified Control.Monad.ST as Strict (ST)
import Control.Monad.ST.Lazy (ST, runST, strictToLazyST)
import Data.Array.Base (getNumElements, unsafeRead, unsafeWrite)
import Data.Array.ST (MArray, STUArray, newArray_, newListArray, )
import Data.Char (digitToInt, intToDigit, isDigit)
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Data.List (findIndex, tails)

game :: forall a s. (Integral a, MArray (STUArray s) a (Strict.ST s)) => ST s [a]
game = (3:) . (7:) <$> (strictToLazyST (newListArray @(STUArray s) (0, 1) [3, 7]) >>= loop 0 1 2)
  where loop i j n a = do
            (s, t) <- strictToLazyST $ (,) <$> unsafeRead a i <*> unsafeRead a j
            let (n', digits) = case (s + t) `divMod` 10 of
                    (0, x) -> (n + 1, [x])
                    (x, y) -> (n + 2, [x, y])
                i' = (i + fromIntegral s + 1) `mod` n'
                j' = (j + fromIntegral t + 1) `mod` n'
            a' <- strictToLazyST $ do
                size <- getNumElements a
                let size':_ = filter (n' <=) $ iterate (* 2) size
                a' <- if size' <= size then return a else do
                    a' <- newArray_ (0, size' - 1)
                    a' <$ sequence_ [unsafeRead a k >>= unsafeWrite a' k | k <- [0..n - 1]]
                a' <$ zipWithM_ (unsafeWrite a') [n..] digits
            (digits ++) <$> loop i' j' n' a'

day14a :: String -> String
day14a (read . filter isDigit -> offset) =
    map (intToDigit . fromIntegral) . take 10 . drop offset $ runST (game @Word8)

day14b :: String -> Int
day14b (map (fromIntegral . digitToInt) . filter isDigit -> digits) =
    fromJust . findIndex and . map (zipWith (==) digits) . tails $ runST (game @Word8)
