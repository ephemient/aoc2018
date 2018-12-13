module Day13Spec (spec) where

import Day13 (day13a, day13b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day13a sampleA `shouldBe` "7,3"
    describe "part 2" $
        it "examples" $
            day13b sampleB `shouldBe` "6,4"
  where sampleA = unlines
          [ "/->-\\        "
          , "|   |  /----\\"
          , "| /-+--+-\\  |"
          , "| | |  | v  |"
          , "\\-+-/  \\-+--/"
          , "  \\------/   "
          ]
        sampleB = unlines
          [ "/>-<\\  "
          , "|   |  "
          , "| /<+-\\"
          , "| | | v"
          , "\\>+</ |"
          , "  |   ^"
          , "  \\<->/"
          ]
