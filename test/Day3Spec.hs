module Day3Spec (spec) where

import Day3 (day3a, day3b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day3a sample `shouldBe` Just 4
    describe "part 2" $
        it "examples" $
            day3b sample `shouldBe` Just 3
  where sample = "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2\n"
