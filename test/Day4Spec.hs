module Day4Spec (spec) where

import Day4 (day4a, day4b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day4a sample `shouldBe` 240
    describe "part 2" $
        it "examples" $
            day4b sample `shouldBe` 4455
  where sample =
            "[1518-11-01 00:00] Guard #10 begins shift\n" ++
            "[1518-11-01 00:05] falls asleep\n" ++
            "[1518-11-01 00:25] wakes up\n" ++
            "[1518-11-01 00:30] falls asleep\n" ++
            "[1518-11-01 00:55] wakes up\n" ++
            "[1518-11-01 23:58] Guard #99 begins shift\n" ++
            "[1518-11-02 00:40] falls asleep\n" ++
            "[1518-11-02 00:50] wakes up\n" ++
            "[1518-11-03 00:05] Guard #10 begins shift\n" ++
            "[1518-11-03 00:24] falls asleep\n" ++
            "[1518-11-03 00:29] wakes up\n" ++
            "[1518-11-04 00:02] Guard #99 begins shift\n" ++
            "[1518-11-04 00:36] falls asleep\n" ++
            "[1518-11-04 00:46] wakes up\n" ++
            "[1518-11-05 00:03] Guard #99 begins shift\n" ++
            "[1518-11-05 00:45] falls asleep\n" ++
            "[1518-11-05 00:55] wakes up\n"
