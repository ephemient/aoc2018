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
import Text.ParserCombinators.ReadP (between, char, readP_to_S, readS_to_P, skipSpaces)

data Area dim = Area { minX :: dim, minY :: dim, maxX :: dim, maxY :: dim }

data Claim id dim = Claim { claimId :: id, claimArea :: Area dim }

instance (Ord dim) => Semigroup (Area dim) where
    a <> b = Area
      { minX = (min `on` minX) a b
      , minY = (min `on` minY) a b
      , maxX = (max `on` maxX) a b
      , maxY = (max `on` maxY) a b
      }

instance (Read id, Num dim, Read dim) => Read (Claim id dim) where
    readsPrec _ = readP_to_S $ do
        char '#'
        claimId <- readS_to_P reads
        between skipSpaces skipSpaces $ char '@'
        minX <- readS_to_P reads
        char ','
        minY <- readS_to_P reads
        between skipSpaces skipSpaces $ char ':'
        w <- readS_to_P reads
        char 'x'
        h <- readS_to_P reads
        return Claim { claimId, claimArea = Area { maxX = minX + w - 1, maxY = minY + h - 1, .. } }

parse :: (Read id, Num dim, Read dim) => String -> [Claim id dim]
parse = map read . lines

ix :: Area dim -> ((dim, dim), (dim, dim))
ix Area {..} = ((minX, minY), (maxX, maxY))

day3a :: String -> Int
day3a (map claimArea . parse @Int -> input@(nonEmpty -> Just (sconcat -> bounds))) =
    length . filter (> 1) . elems $ runSTUArray $ do
    a <- newArray (ix bounds) (0 :: Int)
    forM_ input $ \area -> forM_ (range $ ix area) $ \i -> succ <$> readArray a i >>= writeArray a i
    return a

day3b :: String -> Maybe Int
day3b (parse -> input@(nonEmpty . map claimArea -> Just (sconcat -> bounds))) =
    fmap fst . minView $ runST $ do
    ids <- newSTRef $ fromList $ claimId <$> input
    a <- newSTArray (ix bounds) Nothing
    forM_ input $ \Claim {..} -> forM_ (range $ ix claimArea) $ \i -> readArray a i >>= \case
        Just claimId' -> modifySTRef ids $ delete claimId' . delete claimId
        Nothing -> writeArray a i $ Just claimId
    readSTRef ids
  where newSTArray :: (Ix i) => (i, i) -> e -> ST s (STArray s i e)
        newSTArray = newArray
