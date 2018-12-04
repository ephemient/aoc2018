{-|
Module:         Day4
Description:    <https://adventofcode.com/2018/day/4 Day 4: Repose Record>
-}
{-# LANGUAGE FlexibleContexts, RecordWildCards, TupleSections, TypeApplications, ViewPatterns #-}
module Day4 (day4a, day4b) where

import Data.List (maximumBy, sort)
import Data.Map.Strict (Map, assocs, fromDistinctAscList, fromListWith, mapKeysMonotonic, unionsWith)
import Data.Ord (comparing)
import Data.Time (ParseTime, UTCTime, defaultTimeLocale, diffUTCTime, parseTimeM, timeToTimeOfDay, todMin, utctDayTime)
import Text.Megaparsec (MonadParsec, between, choice, many, parseMaybe, sepEndBy)
import Text.Megaparsec.Char (char, newline, noneOf, space, string)
import Text.Megaparsec.Char.Lexer (decimal)

data Event id = Owner { owner :: id } | Start | End deriving (Eq, Ord)

parser :: (MonadParsec e String m, ParseTime ts, Integral id) => m [(ts, Event id)]
parser = flip sepEndBy newline $ do
    Just ts <- iso8601ParseM <$> between (char '[') (char ']') (many $ noneOf "[]") <* space
    (ts,) <$> choice
      [ Owner <$> between (string "Guard #") (string " begins shift") decimal
      , Start <$ string "falls asleep"
      , End <$ string "wakes up"
      ]
  where iso8601ParseM = parseTimeM False defaultTimeLocale "%Y-%m-%d %H:%M"

schedule :: (Ord ts, Ord id) => [(ts, Event id)] -> [(id, ts, ts)]
schedule = schedule' (error "missing owner") Nothing . sort
  where schedule' _ _ [] = []
        schedule' _ Nothing ((_, Owner {..}):rest) = schedule' owner Nothing rest
        schedule' owner Nothing ((t0, Start):rest) = schedule' owner (Just t0) rest
        schedule' owner (Just t0) ((t1, End):rest) = (owner, t0, t1):schedule' owner Nothing rest
        schedule' _ _ _ = error "invalid state"

countMinutes :: UTCTime -> UTCTime -> Map Int Int
countMinutes t0@(todMin . timeToTimeOfDay . utctDayTime -> m0) t1 =
    fromDistinctAscList [(m, minutes `div` 60 + fromEnum (interval m)) | m <- [0..59]]
  where minutes = truncate $ diffUTCTime t1 t0 / 60
        m1 = (m0 + minutes) `mod` 60
        interval m = (if m0 < m1 then (&&) else (||)) (m0 <= m) (m < m1)

day4a :: String -> Int
day4a (parseMaybe @() parser -> Just (schedule -> intervals)) = maxOwner * maxMinute
  where (maxOwner, _) = maximumBy (comparing snd) $ assocs $ fromListWith (+)
            [(owner, diffUTCTime t1 t0) | (owner, t0, t1) <- intervals]
        (maxMinute, _) = maximumBy (comparing snd) $ assocs $ unionsWith (+)
            [countMinutes t0 t1 | (owner, t0, t1) <- intervals, owner == maxOwner]

day4b :: String -> Int
day4b (parseMaybe @() parser -> Just (schedule -> intervals)) = maxOwner * maxMinute
  where ((maxOwner, maxMinute), _) = maximumBy (comparing snd) $ assocs $ unionsWith (+)
            [mapKeysMonotonic (owner,) $ countMinutes t0 t1 | (owner, t0, t1) <- intervals]
