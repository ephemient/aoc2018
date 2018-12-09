{-|
Module:         Day9
Description:    <https://adventofcode.com/2018/day/9 Day 9: Marble Mania>
-}
{-# LANGUAGE FlexibleContexts, NamedFieldPuns, PatternGuards, RecordWildCards, StrictData, TypeApplications, ViewPatterns #-}
module Day9 (day9a, day9b, play) where

import Control.Arrow (second)
import Data.IntMap (IntMap, elems, empty, insertWith)
import Data.List (foldl')
import Text.Megaparsec (MonadParsec, parseMaybe, skipManyTill, takeRest)
import Text.Megaparsec.Char (satisfy)
import Text.Megaparsec.Char.Lexer (decimal)

data Zipper a = Zipper {left :: [a], here :: a, right :: [a]}

data Game a = Game {scores :: IntMap a, ring :: Zipper a}

parser :: (MonadParsec e String m, Integral players, Integral target) => m (players, target)
parser = (,) <$> decimal <*> skipManyTill (satisfy $ const True) decimal <* takeRest

singleton :: a -> Zipper a
singleton here = Zipper {left = [], here, right = []}

moveZipper :: Int -> Zipper a -> Zipper a
moveZipper n zipper@Zipper {..} = case compare n 0 of
    EQ -> zipper
    _  | [] <- left, [] <- right -> zipper
    LT | (reverse -> a:as, bs) <- splitAt (-n) left ->
         moveZipper (n + 1 + length as) zipper {left = bs, here = a, right = as ++ (here:right)}
       | otherwise -> moveZipper n zipper {left = reverse right, right = []}
    GT | (reverse -> a:as, bs) <- splitAt n right ->
         moveZipper (n - 1 - length as) zipper {left = as ++ (here:left), here = a, right = bs}
       | otherwise -> moveZipper n zipper {left = [], right = reverse left}

insertZipper :: a -> Zipper a -> Zipper a
insertZipper a zipper@Zipper {..} = zipper {left = here:left, here = a}

removeZipper :: Zipper a -> (a, Zipper a)
removeZipper Zipper {left = [], right = []} = error "empty"
removeZipper zipper@Zipper {..} = case right of
    a:as -> (here, zipper {here = a, right = as})
    [] -> removeZipper zipper {left = [], right = reverse left}

play :: Int -> Int -> Int
play players target = maximum . elems . scores $
    foldl' acc Game {scores = empty, ring = singleton 0} [1..target]
  where acc game@Game {..} n
          | n `mod` 23 == 0
          = let scores' = insertWith (+) (n `mod` players) (n + m) scores
                (m, ring') = removeZipper $ moveZipper (-7) ring
            in game {scores = scores', ring = ring'}
          | otherwise
          = game {ring = insertZipper n $ moveZipper 1 ring}

day9a :: String -> Maybe Int
day9a = fmap (uncurry play) . parseMaybe @() parser

day9b :: String -> Maybe Int
day9b = fmap (uncurry play . second (* 100)) . parseMaybe @() parser
