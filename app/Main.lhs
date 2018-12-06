# [Advent of Code 2018](https://adventofcode.com/2018)
### my answers in [Haskell](https://www.haskell.org/) (see also [Kotlin branch](https://github.com/ephemient/aoc2018/tree/kotlin))

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
    run 4 getDayInput print [day4a, day4b]
    run 5 getDayInput print [day5a, day5b]
    run 6 getDayInput print [day6a, day6b 10000]
```
