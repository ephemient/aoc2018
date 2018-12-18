module Day18Spec (spec) where

import Day18 (day18)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = it "example" $ day18 10 sample `shouldBe` 1147
  where sample = unlines
          [ ".#.#...|#."
          , ".....#|##|"
          , ".|..|...#."
          , "..|#.....#"
          , "#.#|||#|#|"
          , "...#.||..."
          , ".|....|..."
          , "||...#|.#|"
          , "|.||||..|."
          , "...#.|..|."
          ]
