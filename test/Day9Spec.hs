module Day9Spec (spec) where

import Day9 (play)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = it "examples" $ do
    play 9 25 `shouldBe` 32
    play 10 1618 `shouldBe` 8317
    play 13 7999 `shouldBe` 146373
    play 17 1104 `shouldBe` 2764
    play 21 6111 `shouldBe` 54718
    play 30 5807 `shouldBe` 37305
