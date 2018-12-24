{-|
Module:         Day24
Description:    <https://adventofcode.com/2018/day/24 Day 24: Immune System Simulator 20XX>
-}
{-# LANGUAGE FlexibleContexts, NamedFieldPuns, RecordWildCards, TransformListComp, TupleSections, TypeApplications #-}
module Day24 (day24a, day24b) where

import Control.Arrow ((&&&))
import Data.Either (partitionEithers)
import Data.List (find, foldl', mapAccumL, sortOn)
import Data.Map.Strict (alter, assocs, fromList, lookup)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Ord (Down(Down))
import Data.Set (empty, insert, member, notMember)
import GHC.Exts (sortWith)
import Prelude hiding (lookup)
import Text.Megaparsec (MonadParsec, (<|>), between, notFollowedBy, optional, parseMaybe, sepBy, sepEndBy, some)
import Text.Megaparsec.Char (alphaNumChar, char, newline, printChar, string)
import Text.Megaparsec.Char.Lexer (decimal)

data Key s a = Key {team :: s, n :: a}
  deriving (Eq, Ord)

data Unit a s = Unit
  { count :: a
  , hp :: a
  , weak :: [s]
  , immune :: [s]
  , power :: a
  , type_ :: s
  , initiative :: Int
  }

parser :: (Integral a, MonadParsec e String m) => m [(Key String Int, Unit a String)]
parser = concat <$> sepEndBy teamP newline
  where teamP = do
            team <- some $ notFollowedBy (char ':' <|> newline) *> printChar
            count <- char ':' *> newline *> sepEndBy unitP newline
            return $ zipWith ((,) . Key team) [1..] count
        unitP = do
            count <- decimal
            hp <- between (string " units each with ") (string " hit points") decimal
            (weak, immune) <-
                fmap (fromMaybe ([], [])) $ optional $ between (string " (") (char ')') $ do
                    (weaks, immunes) <- fmap partitionEithers $ flip sepBy (string "; ") $
                        (Left <$ string "weak to " <|> Right <$ string "immune to ") <*>
                        sepBy (some alphaNumChar) (string ", ")
                    return (concat weaks, concat immunes)
            power <- string " with an attack that does " *> decimal
            type_ <- char ' ' *> some alphaNumChar
            initiative <- string " damage at initiative " *> decimal
            return Unit {..}

step :: (Ord k, Ord i, Integral a, Ord a, Eq s) => [(Key k i, Unit a s)] -> [(Key k i, Unit a s)]
step units = assocs $ foldl' attack (fromList units) attacks
  where attacks = map snd $ sortOn (Down . fst) $ catMaybes $ snd $ mapAccumL buildAttack empty
          [ (key, effectivePower, type_, initiative)
          | (key, Unit {count, power, type_, initiative}) <- units
          , let effectivePower = count * power
          , then sortWith by Down (effectivePower, initiative)
          ]
        buildAttack used (src, effectivePower, srcType, srcInitiative) =
            maybe (used, Nothing) ((`insert` used) &&& Just . (srcInitiative,) . (src,)) $
            listToMaybe
              [ dst
              | (dst, Unit {count, power, weak, immune, initiative}) <- units
              , team src /= team dst
              , dst `notMember` used
              , srcType `notElem` immune
              , let damage = (if srcType `elem` weak then 2 else 1) * effectivePower
              , then sortWith by Down (damage, count * power, initiative)
              ]
        attack units' (src, dst)
          | Just Unit {count, power, type_} <- lookup src units'
          = alter (strike type_ $ count * power) dst units'
          | otherwise = units'
        strike type_ effectivePower (Just dst@Unit {count, hp, weak})
          | count > killed = Just dst {count = count - killed}
          where killed = (if type_ `elem` weak then 2 else 1) * effectivePower `div` hp
        strike _ _ _ = Nothing

run :: (Ord k, Ord i, Integral a, Ord a, Eq s) => [(Key k i, Unit a s)] -> [(Key k i, Unit a s)]
run = run' empty
  where run' seen units
          | key `member` seen = units
          | otherwise = run' (insert key seen) $ step units
          where key = [(team, n, count) | (Key {team, n}, Unit {count}) <- units]

day24a :: String -> Maybe Int
day24a input = do
    units <- parseMaybe @() (parser @Int) input
    return $ sum $ count . snd <$> run units

day24b :: String -> Maybe Int
day24b input = do
    units <- parseMaybe @() (parser @Int) input
    let target = "Immune System"
    units' <- find (all $ (== target) . team . fst)
      [ run
          [ (k, if team == target then unit {power = power + boost} else unit)
          | (k@Key {team}, unit@Unit {power}) <- units
          ]
      | boost <- [0..]
      ]
    return $ sum $ count . snd <$> units'
