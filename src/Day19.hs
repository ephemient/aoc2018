{-|
Module:         Day19
Description:    <https://adventofcode.com/2018/day/19 Day 19: Go With The Flow>
-}
{-# LANGUAGE FlexibleContexts, NamedFieldPuns, PatternGuards, RecordWildCards, TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Day19 (day19) where

import Control.Monad (join)
import Data.Array.Unboxed (Array, IArray, Ix, UArray, (!), (//), bounds, inRange, listArray)
import Data.Bits (Bits, (.&.), (.|.))
import Data.Bool (bool)
import Data.List (genericLength)
import Data.Maybe (listToMaybe)
import Text.Megaparsec (MonadParsec, between, choice, parseMaybe, sepEndBy)
import Text.Megaparsec.Char (newline, space, string)
import Text.Megaparsec.Char.Lexer (decimal)

data Op
  = ADDR | ADDI | MULR | MULI | BANR | BANI | BORR | BORI
  | SETR | SETI | GTIR | GTRI | GTRR | EQIR | EQRI | EQRR

data Instruction i = Instruction {op :: Op, a :: i, b :: i, c :: i}

parser :: (IArray a (Instruction i), MonadParsec e String m, Integral i, Ix i) => m (i, a i (Instruction i))
parser = do
    ip <- between (string "#ip" *> space) newline decimal
    isns <- flip sepEndBy newline $ do
        op <- choice
          [ ADDR <$ string "addr", ADDI <$ string "addi"
          , MULR <$ string "mulr", MULI <$ string "muli"
          , BANR <$ string "banr", BANI <$ string "bani"
          , BORR <$ string "borr", BORI <$ string "bori"
          , SETR <$ string "setr", SETI <$ string "seti"
          , GTIR <$ string "gtir", GTRI <$ string "gtri", GTRR <$ string "gtrr"
          , EQIR <$ string "eqir", EQRI <$ string "eqri", EQRR <$ string "eqrr"
          ]
        Instruction op <$> (space *> decimal) <*> (space *> decimal) <*> (space *> decimal)
    return (ip, listArray (0, genericLength isns - 1) isns)

doOp :: (IArray a i, Bits i, Integral i, Ix i) => a i i -> Op -> i -> i -> i
doOp r ADDR a b = r ! a + r ! b
doOp r ADDI a b = r ! a + b
doOp r MULR a b = r ! a * r ! b
doOp r MULI a b = r ! a * b
doOp r BANR a b = r ! a .&. r ! b
doOp r BANI a b = r ! a .&. b
doOp r BORR a b = r ! a .|. r ! b
doOp r BORI a b = r ! a .|. b
doOp r SETR a _ = r ! a
doOp _ SETI a _ = a
doOp r GTIR a b = bool 0 1 $ a > r ! b
doOp r GTRI a b = bool 0 1 $ r ! a > b
doOp r GTRR a b = bool 0 1 $ r ! a > r ! b
doOp r EQIR a b = bool 0 1 $ a == r ! b
doOp r EQRI a b = bool 0 1 $ r ! a == b
doOp r EQRR a b = bool 0 1 $ r ! a == r ! b

sumFactors :: (Integral a) => a -> a
sumFactors n = sum
  [ d + if d == q then 0 else q
  | d <- takeWhile ((<= n) . join (*)) [1..]
  , let (q, r) = n `divMod` d
  , r == 0
  ]

step :: (IArray a1 i, IArray a2 (Instruction i), Bits i, Integral i, Ix i) => i -> a2 i (Instruction i) -> a1 i i -> a1 i i
step ip isns regs
  | base <- regs ! ip, inRange (bounds isns) (base + 14)
  , Instruction SETI 1 _ i <- isns ! base, i /= ip
  , Instruction SETI 1 _ j <- isns ! (base + 1), j `notElem` [ip, i]
  , Instruction MULR i' j' k <- isns ! (base + 2), i == i', j == j', k `notElem` [ip, i, j]
  , Instruction EQRR k' n k'' <- isns ! (base + 3), k == k', k == k'', n `notElem` [ip, i, j, k]
  , Instruction ADDR k' ip' ip'' <- isns ! (base + 4), k == k', ip == ip', ip == ip''
  , Instruction ADDI ip' 1 ip'' <- isns ! (base + 5), ip == ip', ip == ip''
  , Instruction ADDR i' 0 m <- isns ! (base + 6), i == i', m `notElem` [ip, i, j, k, n]
  , Instruction ADDI j' 1 j'' <- isns ! (base + 7), j == j', j == j''
  , Instruction GTRR j' n' k' <- isns ! (base + 8), j == j', n == n', k == k'
  , Instruction ADDR ip' k' ip'' <- isns ! (base + 9), ip == ip', k == k', ip == ip''
  , Instruction SETI base' _ ip' <- isns ! (base + 10), base + 1 == base', ip == ip'
  , Instruction ADDI i' 1 i'' <- isns ! (base + 11), i == i', i == i''
  , Instruction GTRR i' n' k' <- isns ! (base + 12), i == i', n == n', k == k'
  , Instruction ADDR k' ip' ip'' <- isns ! (base + 13), k == k', ip == ip', ip == ip''
  , Instruction SETI base' _ ip' <- isns ! (base + 14), base == base', ip == ip'
  , goal <- regs ! n, goal > 0
  = regs // [(ip, base + 15), (i, goal + 1), (j, goal + 1), (k, 1), (m, regs ! m + sumFactors goal)]
  | otherwise = regs // if ip == c then [(ip, result + 1)] else [(ip, regs ! ip + 1), (c, result)]
  where Instruction {..} = isns ! (regs ! ip)
        result = doOp regs op a b

day19 :: Int -> String -> Maybe Int
day19 z input = do
    (ip, isns) <- parseMaybe @() (parser @Array) input
    regs <- listToMaybe $ dropWhile (inRange (bounds isns) . (! ip)) $ iterate (step ip isns) $
        listArray @UArray (0, 5) $ z : repeat 0
    return $ regs ! 0
