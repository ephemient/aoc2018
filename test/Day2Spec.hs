module Day2Spec (spec) where

import Day2 (day2a, day2b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day2a "abcdef\nbababc\nabbcde\nabcccd\naabcdd\nabcdee\nababab" `shouldBe` 12
    describe "part 2" $
        it "examples" $
            day2b "abcde\nfghij\nklmno\npqrst\nfguij\naxcye\nwvxyz" `shouldBe` Just "fgij"
