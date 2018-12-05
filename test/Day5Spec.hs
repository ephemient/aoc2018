module Day5Spec (spec) where

import Day5 (day5a, day5b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day5a "dabAcCaCBAcCcaDA" `shouldBe` 10
    describe "part 2" $
        it "examples" $
            day5b "dabAcCaCBAcCcaDA" `shouldBe` 4
