module Day11Spec (spec) where

import Day11 (day11a, day11b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $ do
            day11a "18" `shouldBe` "33,45"
            day11a "42" `shouldBe` "21,61"
    describe "part 2" $
        it "examples" $ do
            day11b "18" `shouldBe` "90,269,16"
            day11b "42" `shouldBe` "232,251,12"
