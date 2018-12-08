module Day8Spec (spec) where

import Day8 (day8a, day8b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day8a sample `shouldBe` Just 138
    describe "part 2" $
        it "examples" $
            day8b sample `shouldBe` Just 66
  where sample = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2\n"
