module Day7Spec (spec) where

import Day7 (day7a, day7b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day7a sample `shouldBe` "CABDFE"
    describe "part 2" $
        it "examples" $
            day7b 0 2 sample `shouldBe` 15
  where sample = unlines
          [ "Step C must be finished before step A can begin."
          , "Step C must be finished before step F can begin."
          , "Step A must be finished before step B can begin."
          , "Step A must be finished before step D can begin."
          , "Step B must be finished before step E can begin."
          , "Step D must be finished before step E can begin."
          , "Step F must be finished before step E can begin."
          ]
