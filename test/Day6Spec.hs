module Day6Spec (spec) where

import Day6 (day6a, day6b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day6a sample `shouldBe` 17
    describe "part 2" $
        it "examples" $
            day6b 32 sample `shouldBe` 16
  where sample = "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9"
