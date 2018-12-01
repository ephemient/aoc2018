module Day1Spec (spec) where

import Day1 (day1a, day1b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $ do
            day1a "+1\n-2\n+3\n+1" `shouldBe` 3
            day1a "+1\n+1\n+1" `shouldBe` 3
            day1a "+1\n+1\n-2" `shouldBe` 0
            day1a "-1\n-2\n-3" `shouldBe` -6
    describe "part 2" $
        it "examples" $ do
            day1b "+1\n-2\n+3\n+1" `shouldBe` 2
            day1b "+1\n-1" `shouldBe` 0
            day1b "+3\n+3\n+4\n-2\n-4" `shouldBe` 10
            day1b "-6\n+3\n+8\n+5\n-6" `shouldBe` 5
            day1b "+7\n+7\n-2\n-7\n-4" `shouldBe` 14
