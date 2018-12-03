{-|
Module:         Day3
Description:    <https://adventofcode.com/2018/day/3 Day 3: No Matter How You Slice It>
-}
{-# LANGUAGE LambdaCase, NamedFieldPuns, RecordWildCards, TypeApplications, ViewPatterns #-}
module Day3 (day3a, day3b) where

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Array.IArray (Ix, elems, range)
import Data.Array.ST (STArray, newArray, readArray, runSTUArray, writeArray)
import Data.Function (on)
import Data.IntSet (delete, fromList, minView)
import Data.List.NonEmpty (nonEmpty)
import Data.STRef (modifySTRef, newSTRef, readSTRef)
import Data.Semigroup (sconcat)
import Text.Megaparsec (between, parseMaybe, sepEndBy)
import Text.Megaparsec.Char (char, newline, space)
import Text.Megaparsec.Char.Lexer (decimal)

data Area dim = Area { minX :: dim, minY :: dim, maxX :: dim, maxY :: dim }

data Claim id dim = Claim { claimId :: id, claimArea :: Area dim }

instance (Ord dim) => Semigroup (Area dim) where
    a <> b = Area
      { minX = (min `on` minX) a b
      , minY = (min `on` minY) a b
      , maxX = (max `on` maxX) a b
      , maxY = (max `on` maxY) a b
      }

parse :: (Integral id, Integral dim) => String -> Maybe [Claim id dim]
parse = parseMaybe @() $ flip sepEndBy newline $ do
    claimId <- char '#' >> decimal
    minX <- spacedChar '@' >> decimal
    minY <- spacedChar ',' >> decimal
    maxX <- spacedChar ':' >> (+ (minX - 1)) <$> decimal
    maxY <- spacedChar 'x' >> (+ (minY - 1)) <$> decimal
    return Claim { claimArea = Area {..}, .. }
  where spacedChar = between space space . char

ix :: Area dim -> ((dim, dim), (dim, dim))
ix Area {..} = ((minX, minY), (maxX, maxY))

day3a :: String -> Maybe Int
day3a (parse @Int -> Just (map claimArea -> input@(nonEmpty -> Just (sconcat -> bounds)))) =
    Just . length . filter (> 1) . elems $ runSTUArray $ do
    a <- newArray (ix @Int bounds) (0 :: Int)
    forM_ input $ \area -> forM_ (range $ ix area) $ \i -> succ <$> readArray a i >>= writeArray a i
    return a
day3a _ = Nothing

day3b :: String -> Maybe Int
day3b (parse -> Just input@(nonEmpty . map claimArea -> Just (sconcat -> bounds))) =
    fmap fst . minView $ runST $ do
    ids <- newSTRef $ fromList $ claimId <$> input
    a <- newSTArray (ix @Int bounds) Nothing
    forM_ input $ \Claim {..} -> forM_ (range $ ix claimArea) $ \i -> readArray a i >>= \case
        Just claimId' -> modifySTRef ids $ delete claimId' . delete claimId
        Nothing -> writeArray a i $ Just claimId
    readSTRef ids
  where newSTArray :: (Ix i) => (i, i) -> e -> ST s (STArray s i e)
        newSTArray = newArray
day3b _ = Nothing
