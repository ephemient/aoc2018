module Day25Spec (spec) where

import Day25 (day25a)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
    describe "part 1" $
        it "examples" $ do
            day25a sample1 `shouldBe` 2
            day25a sample2 `shouldBe` 4
            day25a sample3 `shouldBe` 3
            day25a sample4 `shouldBe` 8
  where sample1 = unlines
          [ " 0,0,0,0"
          , " 3,0,0,0"
          , " 0,3,0,0"
          , " 0,0,3,0"
          , " 0,0,0,3"
          , " 0,0,0,6"
          , " 9,0,0,0"
          , "12,0,0,0"
          ]
        sample2 = unlines
          [ "-1,2,2,0"
          , "0,0,2,-2"
          , "0,0,0,-2"
          , "-1,2,0,0"
          , "-2,-2,-2,2"
          , "3,0,2,-1"
          , "-1,3,2,2"
          , "-1,0,-1,0"
          , "0,2,1,-2"
          , "3,0,0,0"
          ]
        sample3 = unlines
          [ "1,-1,0,1"
          , "2,0,-1,0"
          , "3,2,-1,0"
          , "0,0,3,1"
          , "0,0,-1,-1"
          , "2,3,-2,0"
          , "-2,2,0,0"
          , "2,-2,0,-1"
          , "1,-1,0,-1"
          , "3,2,0,2"
          ]
        sample4 = unlines
          [ "1,-1,-1,-2"
          , "-2,-2,0,1"
          , "0,2,1,3"
          , "-2,3,-2,1"
          , "0,2,3,-2"
          , "-1,-1,1,-2"
          , "0,-2,-1,0"
          , "-2,2,3,-1"
          , "1,2,2,0"
          , "-1,-2,0,-2"
          ]
