{-|
Module:         Day21
Description:    <https://adventofcode.com/2018/day/21 Day 21: Chronal Conversion>
-}
{-# LANGUAGE FlexibleContexts, RecordWildCards, TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Day21 (day21a, day21b) where

import Control.Monad.Cont (callCC, runCont, runContT)
import Control.Monad.Loops (iterateM_)
import Control.Monad.State (evalState, get, put)
import Data.Array.Unboxed (Array, IArray, Ix, UArray, (!), (//), bounds, inRange, listArray)
import Data.Bits (Bits, (.&.), (.|.))
import Data.Bool (bool)
import qualified Data.IntSet as S (empty, insert, member)
import Data.List (genericLength)
import Text.Megaparsec (MonadParsec, between, choice, parseMaybe, sepEndBy)
import Text.Megaparsec.Char (newline, space, string)
import Text.Megaparsec.Char.Lexer (decimal)

data Op
  = ADDR | ADDI | MULR | MULI | BANR | BANI | BORR | BORI
  | SETR | SETI | GTIR | GTRI | GTRR | EQIR | EQRI | EQRR
  deriving (Eq)

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

step :: (Monad m, IArray a1 i, IArray a2 (Instruction i), Bits i, Integral i, Ix i, Show (a1 i i)) => (i -> m ()) -> i -> a2 i (Instruction i) -> a1 i i -> m (a1 i i)
step f ip isns regs
  | c == 0 = fail "writing value to register 0"
  | op == EQRR, a == 0, c /= ip = regs // [(c, 0), (ip, regs ! ip + 1)] <$ f (regs ! b)
  | op == EQRR, b == 0, c /= ip = regs // [(c, 0), (ip, regs ! ip + 1)] <$ f (regs ! a)
  | (op /= SETI && a == 0) || (op `elem` [ADDR, MULR, BANR, BORR, GTIR, GTRR, EQIR, EQRR] && b == 0)
  = fail "reading from register 0"
  | inRange (bounds isns) (base + 8)
  , Instruction SETI 0 _ t <- isn, t `notElem` [0, ip]
  , Instruction ADDI t' 1 u <- isns ! (base + 1), t == t', u `notElem` [0, ip, t]
  , Instruction MULI u' n u'' <- isns ! (base + 2), u == u', n > 0, u == u''
  , Instruction GTRR u' r u'' <- isns ! (base + 3), u == u', r `notElem` [0, ip, t], u == u''
  , Instruction ADDR u' ip' ip'' <- isns ! (base + 4), u == u', ip == ip', ip == ip''
  , Instruction ADDI ip' 1 ip'' <- isns ! (base + 5), ip == ip', ip == ip''
  , Instruction SETI base' _ ip' <- isns ! (base + 6), base + 8 == base', ip == ip'
  , Instruction ADDI t' u' t'' <- isns ! (base + 7), t == t', u == u', t == t''
  , Instruction SETI base' _ ip' <- isns ! (base + 8), base == base', ip == ip'
  = return $ regs // [(ip, base + 9), (t, max 0 $ regs ! r `div` n), (u, 1)]
  | otherwise
  = return $ regs // if ip == c then [(ip, result + 1)] else [(ip, regs ! ip + 1), (c, result)]
  where base = regs ! ip
        isn@Instruction {..} = isns ! base
        result = doOp regs op a b

day21a :: String -> Maybe Int
day21a input = do
    (ip, isns) <- parseMaybe @() (parser @Array) input
    let regs = listArray @UArray (0, 5) $ repeat 0
    return $ flip runCont id $ callCC $ \f -> iterateM_ (step f ip isns) regs

day21b :: String -> Maybe Int
day21b input = do
    (ip, isns) <- parseMaybe @() (parser @Array) input
    let regs = listArray @UArray (0, 5) $ repeat 0
        reportDup f i = do
            (seen, prior) <- get
            if i `S.member` seen then f prior else put (S.insert i seen, i)
    return $ flip evalState (S.empty, undefined) $ flip runContT return $ callCC $ \f ->
        iterateM_ (step (reportDup f) ip isns) regs
