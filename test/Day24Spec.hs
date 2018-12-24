module Day24Spec (spec) where

import Day24 (day24a, day24b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day24a sample `shouldBe` Just 5216
    describe "part 2" $
        it "examples" $
            day24b sample `shouldBe` Just 51
  where sample = unlines
          [ "Immune System:"
          , "17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2"
          , "989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3"
          , ""
          , "Infection:"
          , "801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1"
          , "4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4"
          ]
