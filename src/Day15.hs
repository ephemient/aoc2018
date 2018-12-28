{-|
Module:         Day15
Description:    <https://adventofcode.com/2018/day/15 Day 15: Beverage Bandits>
-}
{-# LANGUAGE LambdaCase, NamedFieldPuns, RecordWildCards, TypeApplications, ViewPatterns #-}
module Day15 (day15a, day15b) where

import Control.Arrow ((&&&), (***))
import Control.Parallel.Strategies (parBuffer, rseq, withStrategy)
import Data.Either (partitionEithers)
import Data.Foldable (asum)
import Data.Functor.Identity (runIdentity)
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NE (fromList)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M ((!?), alterF, elems, empty, filter, findWithDefault, fromDistinctAscList, fromListWith, insert, keys, keysSet, minViewWithKey, null, restrictKeys, toAscList, toList, union)
import Data.Maybe (fromJust)
import Data.Semigroup (Max(Max), Min(Min), sconcat)
import Data.Set (Set)
import qualified Data.Set as S ((\\), fromDistinctAscList, intersection, lookupMin, member, null, singleton, toList, union, unions)
import GHC.Conc (numCapabilities)

data Species = Elf | Goblin deriving (Eq)
data Unit e = Unit {species :: Species, hp :: e}
data Cave i e = Cave {walls :: Set (i, i), units :: Map (i, i) (Unit e)}

caveFromString :: (Integral i) => (Species -> e) -> String -> Cave i e
caveFromString initialHP string =
    uncurry Cave . (S.fromDistinctAscList *** M.fromDistinctAscList) . partitionEithers $ do
        (y, line) <- zip [0..] $ lines string
        (x, c) <- zip [0..] line
        case c of
            '#' -> [Left (y, x)]
            'E' -> [Right ((y, x), Unit Elf $ initialHP Elf)]
            'G' -> [Right ((y, x), Unit Goblin $ initialHP Goblin)]
            _ -> []

caveToDebugString :: (Integral i, Show e) => Cave i e -> String
caveToDebugString Cave {..} = unlines $ map rowToDebugString [y0..y1]
  where ((Min y0, Max y1), (Min x0, Max x1)) = sconcat . NE.fromList $
            ((Min &&& Max) *** (Min &&& Max)) <$> S.toList walls
        rowToDebugString y =
            map (caveChar y) [x0..x1] ++ "  " ++ M.findWithDefault "" y unitsByLine
        caveChar y x
          | S.member (y, x) walls = '#'
          | Just Unit {species = Elf} <- units M.!? (y, x) = 'E'
          | Just Unit {species = Goblin} <- units M.!? (y, x) = 'G'
          | otherwise = '.'
        unitsByLine = M.fromListWith (\a b -> b ++ ", " ++ a)
          [ (y, (case species of Elf -> 'E'; Goblin -> 'G') : '(' : shows hp ")")
          | ((y, _), Unit {..}) <- M.toAscList units
          ]

adjacencies :: (Num a) => (a, a) -> Set (a, a)
adjacencies (y, x) = S.fromDistinctAscList [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]

nearest :: (Num a, Ord a) => Set (a, a) -> Set (a, a) -> (a, a) -> Maybe (a, a)
nearest walls goals = nearest' walls . S.singleton
  where nearest' visited q
          | S.null q = Nothing
          | reached <- S.intersection goals q, not $ S.null reached = S.lookupMin reached
          | otherwise = nearest' visited' $ S.unions (adjacencies <$> S.toList q) S.\\ visited
          where visited' = S.union visited q

step :: (Monad m, Num i, Ord i, Ord e) => (Maybe (Unit e) -> m (Maybe (Unit e))) -> Set (i, i) -> Map (i, i) (Unit e) -> m (Map (i, i) (Unit e), Bool)
step strike walls = step' M.empty
  where step' past (M.minViewWithKey -> Just ((k, unit), future))
          | M.null enemies = return (M.insert k unit allOtherUnits, False)
          | otherwise = case M.restrictKeys enemies $ adjacencies k' of
                (sortOn (hp . snd) . M.toList -> (target, _):_) -> do
                    past' <- M.insert k' unit <$> M.alterF strike target past
                    future' <- M.alterF strike target future
                    step' past' future'
                _ -> step' (M.insert k' unit past) future
          where
            allOtherUnits = M.union past future
            walls' = S.union walls $ M.keysSet allOtherUnits
            enemies = M.filter ((/= species unit) . species) allOtherUnits
            adjacentEnemies = adjacencies k `S.intersection` M.keysSet enemies
            enemyRanges = S.unions (adjacencies <$> M.keys enemies) S.\\ walls'
            k' = case if S.null adjacentEnemies then nearest walls' enemyRanges k else Nothing of
                Just goal | Just move <- nearest walls' (adjacencies k) goal -> move
                _ -> k
        step' past _ = return (past, True)

outcome :: (Monad m, Num i, Ord i, Num e, Ord e) => (Maybe (Unit e) -> m (Maybe (Unit e))) -> Cave i e -> m (Int, e)
outcome strike Cave {..} = outcome' 0 units
  where outcome' rounds units' = step strike walls units' >>= \case
            (units'', False) -> return (rounds, sum $ map hp $ M.elems units'')
            (units'', True) -> outcome' (rounds + 1) units''

day15a :: String -> (Int, Int)
day15a (caveFromString @Int (const 200) -> cave) = runIdentity $ outcome strike cave
  where strike (Just unit@Unit {hp}) | hp > 3 = return $ Just unit {hp = hp - 3}
        strike _ = return Nothing

day15b :: String -> (Int, (Int, Int))
day15b (caveFromString @Int (const 200) -> cave) = fromJust $ asum $ parallelize
    [(,) elfPower <$> outcome (strike elfPower) cave | elfPower <- [3..]]
  where strike _ (Just unit@Unit {species = Elf, hp})
          | hp > 3 = return $ Just unit {hp = hp - 3}
          | otherwise = fail "oh noes!"
        strike elfPower (Just unit@Unit {hp})
          | hp > elfPower = return $ Just unit {hp = hp - elfPower}
        strike _ _ = return Nothing
        parallelize = withStrategy $ parBuffer numCapabilities rseq
