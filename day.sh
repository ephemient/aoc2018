#!/bin/bash
. "$(git --exec-path)/git-sh-setup" || exit $?
set -euxo pipefail
DAY=${1}
grep -F -w -e "Day${DAY}" -q app/Main.lhs || sed -i -e "
    /^## /,/^---/ {
        /^\$/ i \\
## [Day ${DAY}: ](/src/Day${DAY}.hs)\\
\`\`\`haskell\\
import Day${DAY} (day${DAY}a, day${DAY}b)\\
\`\`\`
    }
    /^main/,\$ {
        /^\`\`\`/ i \\    run ${DAY} getDayInput print [day${DAY}a, day${DAY}b]
    }
" app/Main.lhs
grep -F -w -e "Day${DAY}" -q bench/Main.hs || sed -i -e "
    /^import Paths/ i \\
import Day${DAY} (day${DAY}a, day${DAY}b)
    /^  ]/ i \\
  , env (getDayInput ${DAY}) \$ \\\\input -> bgroup \"Day ${DAY}\"\\
      [ bench \"part 1\" \$ nf day${DAY}a input\\
      , bench \"part 2\" \$ nf day${DAY}b input\\
      ]
" bench/Main.hs
[[ -e "src/Day${DAY}.hs" ]] || cat >"src/Day${DAY}.hs" <<EOF
{-|
Module:         Day${DAY}
Description:    <https://adventofcode.com/2018/day/${DAY} Day ${DAY}: >
-}
{-# LANGUAGE FlexibleContexts, GADTs, LambdaCase, NamedFieldPuns, NoMonomorphismRestriction, ParallelListComp, PatternGuards, RecordWildCards, TransformListComp, TupleSections, TypeApplications, TypeFamilies, ViewPatterns #-}
module Day${DAY} (day${DAY}a, day${DAY}b) where

import Control.Applicative hiding (many, some)
import Control.Arrow
import Control.Monad
import Data.Array
import Data.Bits
import Data.Bool
import Data.Char
import Data.Either
import Data.Function
import Data.Functor
import Data.List
import qualified Data.Map.Lazy as Map
import Data.Maybe
import Data.Monoid
import Data.Ord
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Time
import Data.Tuple
import Data.Word
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Printf

day${DAY}a :: String -> Int
day${DAY}a = const 0

day${DAY}b :: String -> Int
day${DAY}b = const 0
EOF
[[ -e "test/Day${DAY}Spec.hs" ]] || cat >"test/Day${DAY}Spec.hs" <<EOF
module Day${DAY}Spec (spec) where

import Day${DAY} (day${DAY}a, day${DAY}b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" \$
        it "examples" \$
            day${DAY}a "" \`shouldBe\` 0
    describe "part 2" \$
        it "examples" \$
            day${DAY}b "" \`shouldBe\` 0
EOF
[[ -e "day${DAY}.txt" ]] || touch "day${DAY}.txt"
git add -N "src/Day${DAY}.hs" "test/Day${DAY}Spec.hs" "day${DAY}.txt"
nohup gvim -p app/Main.lhs bench/Main.hs "src/Day${DAY}.hs" "test/Day${DAY}Spec.hs" "day${DAY}.txt" +3tabn 0<&- &>/dev/null & disown
stack ghci "src/Day${DAY}.hs"
