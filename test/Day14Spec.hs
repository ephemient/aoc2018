module Day14Spec (spec) where

import Day14 (day14a, day14b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $ do
            day14a "9" `shouldBe` "5158916779"
            day14a "5" `shouldBe` "0124515891"
            day14a "18" `shouldBe` "9251071085"
            day14a "2018" `shouldBe` "5941429882"
    describe "part 2" $
        it "examples" $ do
            day14b "51589" `shouldBe` 9
            day14b "01245" `shouldBe` 5
            day14b "92510" `shouldBe` 18
            day14b "59414" `shouldBe` 2018
