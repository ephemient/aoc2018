module Day12Spec (spec) where

import Day12 (day12)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = it "example" $ day12 20 sample `shouldBe` Just 325
  where sample = unlines
          [ "initial state: #..#.#..##......###...###"
          , ""
          , "...## => #"
          , "..#.. => #"
          , ".#... => #"
          , ".#.#. => #"
          , ".#.## => #"
          , ".##.. => #"
          , ".#### => #"
          , "#.#.# => #"
          , "#.### => #"
          , "##.#. => #"
          , "##.## => #"
          , "###.. => #"
          , "###.# => #"
          , "####. => #"
          ]
