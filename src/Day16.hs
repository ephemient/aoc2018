{-|
Module:         Day16
Description:    <https://adventofcode.com/2018/day/16 Day 16: Chronal Classification>
-}
{-# LANGUAGE FlexibleContexts, RecordWildCards, TypeApplications #-}
module Day16 (day16a, day16b) where

import Control.Arrow ((&&&))
import Control.Monad (foldM)
import Data.Array.Unboxed (IArray, UArray, Ix, (!), (//), listArray)
import Data.Bits (Bits, (.&.), (.|.))
import Data.Bool (bool)
import Data.List (genericLength)
import Data.Map.Lazy ((!?), elems, fromListWith, union)
import qualified Data.Map.Lazy as M (empty, null, partition)
import Data.Set (difference, findMin, fromList, intersection, size, unions)
import Text.Megaparsec (MonadParsec, between, count, many, parseMaybe, sepBy, sepEndBy1)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

data Op
  = ADDR | ADDI | MULR | MULI | BANR | BANI | BORR | BORI
  | SETR | SETI | GTIR | GTRI | GTRR | EQIR | EQRI | EQRR
  deriving (Bounded, Enum, Eq, Ord)

data Sample a op i = Sample
  { sampleOp :: op
  , sampleA :: i
  , sampleB :: i
  , sampleC :: i
  , sampleR0 :: a i i
  , sampleR1 :: a i i
  }

unknownInstructionParser :: (MonadParsec e String m, Integral op, Integral i) => m (op, i, i, i)
unknownInstructionParser = do
    op <- decimal
    [a, b, c] <- count 3 $ char ' ' *> decimal
    return (op, a, b, c)

sampleParser :: (MonadParsec e String m, Integral op, Integral i, Ix i, IArray a i) => m (Sample a op i)
sampleParser = do
    r0 <- between (string "Before: [") (string "]") (sepBy decimal (string ", ")) <* newline
    (sampleOp, sampleA, sampleB, sampleC) <- unknownInstructionParser <* newline
    r1 <- between (string "After:  [") (string "]") (sepBy decimal (string ", ")) <* newline
    return Sample
      { sampleR0 = listArray (0, genericLength r0 - 1) r0
      , sampleR1 = listArray (0, genericLength r1 - 1) r1
      , ..
      }

parser :: (MonadParsec e String m, Integral op, Integral i, Ix i, IArray a i) => m ([Sample a op i], [(op, i, i, i)])
parser = (,)
    <$> (sepEndBy1 sampleParser newline <* many newline)
    <*> sepEndBy1 unknownInstructionParser newline

doOp :: (IArray a i, Bits i, Ix i, Num i) => Op -> i -> i -> i -> a i i -> a i i
doOp ADDR a b c r = r // [(c, r ! a + r ! b)]
doOp ADDI a b c r = r // [(c, r ! a + b)]
doOp MULR a b c r = r // [(c, r ! a * r ! b)]
doOp MULI a b c r = r // [(c, r ! a * b)]
doOp BANR a b c r = r // [(c, r ! a .&. r ! b)]
doOp BANI a b c r = r // [(c, r ! a .&. b)]
doOp BORR a b c r = r // [(c, r ! a .|. r ! b)]
doOp BORI a b c r = r // [(c, r ! a .|. b)]
doOp SETR a _ c r = r // [(c, r ! a)]
doOp SETI a _ c r = r // [(c, a)]
doOp GTIR a b c r = r // [(c, bool 0 1 $ a > r ! b)]
doOp GTRI a b c r = r // [(c, bool 0 1 $ r ! a > b)]
doOp GTRR a b c r = r // [(c, bool 0 1 $ r ! a > r ! b)]
doOp EQIR a b c r = r // [(c, bool 0 1 $ a == r ! b)]
doOp EQRI a b c r = r // [(c, bool 0 1 $ r ! a == b)]
doOp EQRR a b c r = r // [(c, bool 0 1 $ r ! a == r ! b)]

validOps :: (IArray a i, Eq (a i i), Bits i, Ix i, Num i) => Sample a op i -> [Op]
validOps Sample {..} =
    [op | op <- [minBound..maxBound], doOp op sampleA sampleB sampleC sampleR0 == sampleR1]

day16a :: String -> Maybe Int
day16a input = do
    (samples, _) <- parseMaybe @() parser input
    return $ length $ filter ambiguous samples
  where ambiguous sample = case validOps @UArray @Int @Int sample of
            _:_:_:_ -> True
            _ -> False

day16b :: String -> Maybe Int
day16b input = do
    (samples, instructions) <- parseMaybe @() parser input
    codings <- re M.empty $ fromListWith intersection $
            (sampleOp &&& fromList . validOps @UArray @Int @Int) <$> samples
    (! 0) <$> foldM (doOp' codings) (listArray @UArray @Int @Int (0, 3) [0, 0, 0, 0]) instructions
  where re done pending
          | M.null pending = return done
          | (done', pending') <- M.partition ((== 1) . size) pending, not $ M.null done' =
                re (union done $ findMin <$> done') $
                    flip difference (unions $ elems done') <$> pending'
          | otherwise = fail "can not reverse engineer instruction encoding"
        doOp' codings r (op, a, b, c) = do
            op' <- codings !? op
            return $ doOp op' a b c r
