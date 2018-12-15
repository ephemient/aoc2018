module Day15Spec (spec) where

import Day15 (day15a, day15b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $ do
            day15a sample0 `shouldBe` (47, 590)
            day15a sample1 `shouldBe` (37, 982)
            day15a sample2 `shouldBe` (46, 859)
            day15a sample3 `shouldBe` (35, 793)
            day15a sample4 `shouldBe` (54, 536)
            day15a sample5 `shouldBe` (20, 937)
    describe "part 2" $
        it "examples" $ do
            day15b sample0 `shouldBe` (15, (29, 172))
            day15b sample2 `shouldBe` (4, (33, 948))
            day15b sample3 `shouldBe` (15, (37, 94))
            day15b sample4 `shouldBe` (12, (39, 166))
            day15b sample5 `shouldBe` (34, (30, 38))
  where
    sample0 = unlines
      [ "#######"
      , "#.G...#"
      , "#...EG#"
      , "#.#.#G#"
      , "#..G#E#"
      , "#.....#"
      , "#######"
      ]
    sample1 = unlines
      [ "#######"
      , "#G..#E#"
      , "#E#E.E#"
      , "#G.##.#"
      , "#...#E#"
      , "#...E.#"
      , "#######"
      ]
    sample2 = unlines
      [ "#######"
      , "#E..EG#"
      , "#.#G.E#"
      , "#E.##E#"
      , "#G..#.#"
      , "#..E#.#"
      , "#######"
      ]
    sample3 = unlines
      [ "#######"
      , "#E.G#.#"
      , "#.#G..#"
      , "#G.#.G#"
      , "#G..#.#"
      , "#...E.#"
      , "#######"
      ]
    sample4 = unlines
      [ "#######"
      , "#.E...#"
      , "#.#..G#"
      , "#.###.#"
      , "#E#G#G#"
      , "#...#G#"
      , "#######"
      ]
    sample5 = unlines
      [ "#########"
      , "#G......#"
      , "#.E.#...#"
      , "#..##..G#"
      , "#...##..#"
      , "#...#...#"
      , "#.G...G.#"
      , "#.....G.#"
      , "#########"
      ]
