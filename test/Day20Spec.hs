module Day20Spec (spec) where

import Day20 (day20a)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
    describe "part 1" $
        it "examples" $ do
            day20a "^WNE$" `shouldBe` 3
            day20a "^ENWWW(NEEE|SSE(EE|N))$" `shouldBe` 10
            day20a "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$" `shouldBe` 18
            day20a "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$" `shouldBe` 23
            day20a "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$" `shouldBe` 31
