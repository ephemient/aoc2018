# [Advent of Code 2018](https://adventofcode.com/2018)
### my answers in [Haskell](https://www.haskell.org/) (see also [Kotlin branch](https://github.com/ephemient/aoc2018/tree/kotlin) and [Python gist](https://gist.github.com/ephemient/f13c80c6a1ccaa0bb6a81554daa5b788))

[![Build Status](https://travis-ci.org/ephemient/aoc2018.svg?branch=master)](https://travis-ci.org/ephemient/aoc2018)

This project builds with [The Haskell Tool Stack](https://haskellstack.org/).

Setup:

```sh
curl -sSL https://get.haskellstack.org/ | sh -s -
stack setup
```

Run the [HSpec](https://hspec.github.io/) test suite:

```sh
stack test aoc2018:test:aoc2018-test
```

Run [criterion](http://www.serpentine.com/criterion/) benchmarks:

```sh
stack bench aoc2018:bench:aoc2018-bench
```

Print solutions for the inputs provided in local data files:

```sh
stack build aoc2018:exe:aoc2018-exe --exec aoc2018-exe
```

Generate [Haddock](https://www.haskell.org/haddock/) API documentation
(rendered at [ephemient.github.io/aoc2018](https://ephemient.github.io/aoc2018)):

```sh
stack haddock aoc2018:lib
```

Run [hlint](https://github.com/ndmitchell/hlint) source code suggestions:

```sh
stack build hlint --exec 'hlint src test bench'
```

---

<!--
```haskell
{-# LANGUAGE NondecreasingIndentation #-}
module Main (main) where
```
-->

## [Day 1: Chronal Calibration](/src/Day1.hs)
```haskell
import Day1 (day1a, day1b)
```
## [Day 2: Inventory Management System](/src/Day2.hs)
```haskell
import Day2 (day2a, day2b)
```
## [Day 3: No Matter How You Slice It](/src/Day3.hs)
```haskell
import Day3 (day3a, day3b)
```
## [Day 4: Repose Record](/src/Day4.hs)
```haskell
import Day4 (day4a, day4b)
```
## [Day 5: Alchemical Reaction](/src/Day5.hs)
```haskell
import Day5 (day5a, day5b)
```
## [Day 6: Chronal Coordinates](/src/Day6.hs)
```haskell
import Day6 (day6a, day6b)
```
## [Day 7: The Sum of Its Parts](/src/Day7.hs)
```haskell
import Day7 (day7a, day7b)
```
## [Day 8: Memory Maneuver](/src/Day8.hs)
```haskell
import Day8 (day8a, day8b)
```
## [Day 9: Marble Mania](/src/Day9.hs)
```haskell
import Day9 (day9a, day9b)
```
## [Day 10: The Stars Align](/src/Day10.hs)
```haskell
import Day10 (day10)
```
## [Day 11: Chronal Charge](/src/Day11.hs)
```haskell
import Day11 (day11a, day11b)
```
## [Day 12: Subterranean Sustainability](/src/Day12.hs)
```haskell
import Day12 (day12)
```
## [Day 13: Mine Cart Madness](/src/Day13.hs)
```haskell
import Day13 (day13a, day13b)
```
## [Day 14: Chocolate Charts](/src/Day14.hs)
```haskell
import Day14 (day14a, day14b)
```
## [Day 15: Beverage Bandits](/src/Day15.hs)
```haskell
import Day15 (day15a, day15b)
```
## [Day 16: Chronal Classification](/src/Day16.hs)
```haskell
import Day16 (day16a, day16b)
```
## [Day 17: Reservoir Research](/src/Day17.hs)
```haskell
import Day17 (day17a, day17b)
```
## [Day 18: Settlers of The North Pole](/src/Day18.hs)
```haskell
import Day18 (day18)
```
## [Day 19: Go With The Flow](/src/Day19.hs)
```haskell
import Day19 (day19)
```
## [Day 20: A Regular Map](/src/Day20.hs)
```haskell
import Day20 (day20a, day20b)
```
## [Day 21: Chronal Conversion](/src/Day21.hs)
```haskell
import Day21 (day21a, day21b)
```
## [Day 22: Mode Maze](/src/Day22.hs)
```haskell
import Day22 (day22a, day22b)
```
## [Day 23: Experimental Emergency Teleportation](/src/Day23.hs)
```haskell
import Day23 (day23a, day23b)
```
## [Day 24: Immune System Simulator 20XX](/src/Day24.hs)
```haskell
import Day24 (day24a, day24b)
```
## [Day 25: Four-Dimensional Adventure](/src/Day25.hs)
```haskell
import Day25 (day25a)
```

---

```haskell
import Control.Monad (when)
import Data.Maybe (mapMaybe)
import Paths_aoc2018 (getDataFileName)
import System.Environment (getArgs)
import Text.Read (readMaybe)

getDayInput :: Int -> IO String
getDayInput i = getDataFileName ("day" ++ show i ++ ".txt") >>= readFile

readDayInput :: (Read a) => Int -> IO a
readDayInput = fmap read . getDayInput

maybeBottom :: (a -> String) -> Maybe a -> String
maybeBottom = maybe "(⊥)"

showError :: (Show a) => (b -> String) -> Either a b -> String
showError = either (\err -> "(" ++ show err ++ ")")

run :: Int -> (Int -> IO a) -> (b -> IO ()) -> [a -> b] -> IO ()
run day readIO showIO funcs = do
    days <- mapMaybe readMaybe <$> getArgs
    when (null days || day `elem` days) $ do
    putStrLn $ "Day " ++ show day
    contents <- readIO day
    mapM_ (showIO . ($ contents)) funcs
    putStrLn ""

main :: IO ()
main = do
    run 1 getDayInput putStrLn [show . day1a, maybeBottom show . day1b]
    run 2 getDayInput putStrLn [show . day2a, maybeBottom id . day2b]
    run 3 getDayInput (putStrLn . maybeBottom show) [day3a, day3b]
    run 4 getDayInput (putStrLn . maybeBottom show) [day4a, day4b]
    run 5 getDayInput print [day5a, day5b]
    run 6 getDayInput (putStrLn . maybeBottom show) [day6a, day6b 10000]
    run 7 getDayInput putStrLn [day7a, show . day7b 60 5]
    run 8 getDayInput (putStrLn . maybeBottom show) [day8a, day8b]
    run 9 getDayInput (putStrLn . maybeBottom show) [day9a, day9b]
    run 10 getDayInput (putStrLn . maybeBottom (\(i, s) -> s ++ show i)) [day10]
    run 11 getDayInput putStrLn [day11a, day11b]
    run 12 getDayInput (putStrLn . maybeBottom show) [day12 20, day12 50000000000]
    run 13 getDayInput putStrLn [day13a, day13b]
    run 14 getDayInput putStrLn [day14a, show . day14b]
    run 15 getDayInput (print . uncurry (*)) [day15a, snd . day15b]
    run 16 getDayInput (putStrLn . maybeBottom show) [day16a, day16b]
    run 17 getDayInput (putStrLn . maybeBottom show) [day17a, day17b]
    run 18 getDayInput print [day18 10, day18 1000000000]
    run 19 getDayInput (putStrLn . maybeBottom show) [day19 0, day19 1]
    run 20 getDayInput print [day20a, day20b]
    run 21 getDayInput (putStrLn . maybeBottom show) [day21a, day21b]
    run 22 getDayInput (putStrLn . maybeBottom show) [day22a, day22b]
    run 23 getDayInput (putStrLn . maybeBottom show) [day23a, day23b]
    run 24 getDayInput (putStrLn . maybeBottom show) [day24a, day24b]
    run 25 getDayInput print [day25a]
```
