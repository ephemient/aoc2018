module Day23Spec (spec) where

import Day23 (day23a, day23b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day23a sampleA `shouldBe` Just 7
    describe "part 2" $
        it "examples" $
            day23b sampleB `shouldBe` Just 36
  where sampleA = unlines
          [ "pos=<0,0,0>, r=4"
          , "pos=<1,0,0>, r=1"
          , "pos=<4,0,0>, r=3"
          , "pos=<0,2,0>, r=1"
          , "pos=<0,5,0>, r=3"
          , "pos=<0,0,3>, r=1"
          , "pos=<1,1,1>, r=1"
          , "pos=<1,1,2>, r=1"
          , "pos=<1,3,1>, r=1"
          ]
        sampleB = unlines
          [ "pos=<10,12,12>, r=2"
          , "pos=<12,14,12>, r=2"
          , "pos=<16,12,12>, r=4"
          , "pos=<14,14,14>, r=6"
          , "pos=<50,50,50>, r=200"
          , "pos=<10,10,10>, r=5"
          ]
