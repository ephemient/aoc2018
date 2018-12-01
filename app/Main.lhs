# [Advent of Code 2018](https://adventofcode.com/2018)
### my answers in [Haskell](https://www.haskell.org/)

[![Build Status](https://travis-ci.org/ephemient/aoc2018.svg)](https://travis-ci.org/ephemient/aoc2018)

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
```
