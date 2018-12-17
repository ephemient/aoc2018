{-|
Module:         Day17
Description:    <https://adventofcode.com/2018/day/17 Day 17: Reservoir Research>
-}
{-# LANGUAGE FlexibleContexts, NondecreasingIndentation, TupleSections, TypeApplications, ViewPatterns #-}
module Day17 (day17a, day17b) where

import Control.Arrow ((&&&), (***))
import Control.Monad (filterM)
import Control.Monad.Loops (andM, dropWhileM, takeWhileM)
import Data.Array.IArray (Array, IArray, Ix, bounds, accumArray, elems, rangeSize)
import Data.Array.ST (MArray, getBounds, thaw, readArray, runSTArray, writeArray)
import Data.Bool (bool)
import Data.List.NonEmpty (nonEmpty)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe, listToMaybe, maybeToList)
import Data.Semigroup (Max(Max), Min(Min), sconcat)
import Data.Tuple (swap)
import Text.Megaparsec (MonadParsec, (<|>), parseMaybe, sepEndBy)
import Text.Megaparsec.Char (newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

data Element = Empty | Wall | Stagnant | Flowing
  deriving (Eq)

debug :: (IArray a Element, Integral i, Ix i) => a (i, i) Element -> String
debug scene@(bounds -> ((_, minX), (_, maxX))) =
    unlines $ map (bool '.' '+' . (== 500)) [minX..maxX] :
    chunksOf (rangeSize (minX, maxX)) (elemChar <$> elems scene)
  where elemChar Empty = '.'
        elemChar Wall = '#'
        elemChar Stagnant = '~'
        elemChar Flowing = '|'

parser :: (MonadParsec e String m, IArray a Element, Integral i, Ix i) => m (a (i, i) Element)
parser = sepEndBy lineParser newline >>= makeScene . concat
  where lineParser = do
            (secondAxis, order) <- ("x=", id) <$ string "y=" <|> ("y=", swap) <$ string "x="
            y <- decimal <* string ", "
            x0 <- string secondAxis *> decimal
            x1 <- string ".." *> decimal
            return [order (y, x) | x <- [x0..x1]]
        makeScene points@(nonEmpty -> Just points') = return $
            accumArray (flip const) Empty ((minY, minX), (maxY, maxX)) $ (, Wall) <$> points
          where ((Min minY, Min minX), (Max maxY, Max maxX)) =
                    sconcat $ ((Min *** Min . pred) &&& (Max *** Max . succ)) <$> points'
        makeScene _ = fail "expecting points"

ranges :: (Num a, Ord a) => [a] -> [(a, a)]
ranges = ranges' Nothing
  where ranges' k [] = maybeToList k
        ranges' Nothing (x:xs) = ranges' (Just (x, x)) xs
        ranges' (Just (l, r)) (x:xs)
          | r + 1 < x = (l, r) : ranges' (Just (x, x)) xs
          | otherwise = ranges' (Just (l, max r x)) xs

flood :: (MArray a Element m, Integral i, Ix i, Show i) => i -> a (i, i) Element -> m Bool
flood startX scene = do
    ((minY, minX), (maxY, maxX)) <- getBounds scene
    let pour y (x0, x1)
          | y > maxY = return False
          | otherwise = do
                empties <- filterM (\x -> (== Empty) <$> readArray scene (y, x)) [x0..x1]
                blocked0 <- andM [(/= Flowing) <$> readArray scene (y, x) | x <- [x0..x1]]
                blocked1 <- and <$> mapM (fill y) (ranges empties)
                return $ blocked0 && blocked1
        fill y (x0, x1) = do
            blocked <- pour (y + 1) (x0, x1)
            if not blocked
            then False <$ sequence_ [writeArray scene (y, x) Flowing | x <- [x0..x1]]
            else do
            let isEmpty x = (== Empty) <$> readArray scene (y, x)
                isSupported x =
                    if y < maxY
                    then (`notElem` [Empty, Flowing]) <$> readArray scene (y + 1, x)
                    else return False
            lefts <- takeWhileM isEmpty [x0 - 1, x0 - 2..minX]
            rights <- takeWhileM isEmpty [x1 + 1, x1 + 2..maxX]
            l <- fromMaybe (last $ x0 : lefts) . listToMaybe <$> dropWhileM isSupported lefts
            r <- fromMaybe (last $ x1 : rights) . listToMaybe <$> dropWhileM isSupported rights
            if l >= x0 && r <= x1
            then True <$ sequence_ [writeArray scene (y, x) Stagnant | x <- [x0..x1]]
            else do
            sequence_ [writeArray scene (y, x) Wall | x <- [x0..x1]]
            blockedL <- fill y (l, x0 - 1)
            blockedR <- fill y (x1 + 1, r)
            let blocked'@(bool Flowing Stagnant -> elt) = blockedL && blockedR
            blocked' <$ sequence_ [writeArray scene (y, x) elt | x <- [l..r]]
    pour minY (startX, startX)

day17a :: String -> Maybe Int
day17a input = do
    scene <- parseMaybe @() parser input :: Maybe (Array (Int, Int) Element)
    let scene' = runSTArray $ thaw scene >>= \a -> a <$ flood 500 a
    return $ length $ filter (`elem` [Stagnant, Flowing]) $ elems scene'

day17b :: String -> Maybe Int
day17b input = do
    scene <- parseMaybe @() parser input :: Maybe (Array (Int, Int) Element)
    let scene' = runSTArray $ thaw scene >>= \a -> a <$ flood 500 a
    return $ length $ filter (== Stagnant) $ elems scene'
