module Day22Spec (spec) where

import Day22 (day22a, day22b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day22a sample `shouldBe` Just 114
    describe "part 2" $
        it "examples" $
            day22b sample `shouldBe` Just 45
  where sample = "depth: 510\ntarget: 10,10\n"
