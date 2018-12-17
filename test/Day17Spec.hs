module Day17Spec (spec) where

import Day17 (day17a, day17b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day17a sample `shouldBe` Just 57
    describe "part 2" $
        it "examples" $
            day17b sample `shouldBe` Just 29
  where sample = unlines
          [ "x=495, y=2..7"
          , "y=7, x=495..501"
          , "x=501, y=3..7"
          , "x=498, y=2..4"
          , "x=506, y=1..2"
          , "x=498, y=10..13"
          , "x=504, y=10..13"
          , "y=13, x=498..504"
          ]
