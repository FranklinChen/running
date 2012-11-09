{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Framework.TH (defaultMainGenerator)

import Test.HUnit
import Test.Framework.Providers.HUnit (testCase)

import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Control.Monad

import qualified Data.List as List
import qualified Data.Char as Char

import qualified Time1 as T

main = $(defaultMainGenerator)

-- Basic HUnit tests

-- Utils
case_splitDigitsAtColon_digits =
  T.splitDigitsAtColon "12:345:678" @?=
  Just [[T.Digit 1, T.Digit 2],
        [T.Digit 3, T.Digit 4, T.Digit 5],
        [T.Digit 6, T.Digit 7, T.Digit 8]]

case_splitDigitsAtColon_not_digits =
  T.splitDigitsAtColon "12:345:6X8" @?= Nothing

generateDigits = listOf1 $ elements ['0'..'9']

digitsToString :: T.DigitString -> String
digitsToString digits = [Char.chr $ Char.ord '0' + d | T.Digit d <- digits]

{-|
  Splitting and joining round trip
-}
prop_round_trip_splitDigitsAtColon_and_intercalate =
  forAll (do
            h <- generateDigits
            m <- generateDigits
            s <- generateDigits
            return $ h ++
                     ":" ++ m ++
                     ":" ++ s
         ) $
  \xs ->
  case T.splitDigitsAtColon xs of
    Nothing         -> False
    Just digitsList -> List.intercalate ":" (map digitsToString digitsList) == xs

-- timeToSeconds success
case_timeToSeconds_00_00_00 =
  T.timeToSeconds "0:00:00" @?= Just 0

case_timeToSeconds_12_34_56 =
  T.timeToSeconds "12:34:56" @?= Just 45296

-- timeToSeconds failure
case_timeToSeconds_bad_chars =
  T.timeToSeconds "0:x0:59" @?= Nothing

case_timeToSeconds_single_second_digit =
  T.timeToSeconds "0:00:8" @?= Nothing

case_timeToSeconds_seconds_out_of_range =
  T.timeToSeconds "0:00:70" @?= Nothing

case_timeToSeconds_single_minute_digit =
  T.timeToSeconds "0:0:59" @?= Nothing

case_timeToSeconds_minutes_out_of_range =
  T.timeToSeconds "0:60:59" @?= Nothing

-- secondsToTime
case_secondsToTime_0 =
  T.secondsToTime 0 @?= "0:00:00"

case_secondsToTime_45296 =
  T.secondsToTime 45296 @?= "12:34:56"

-- formatMinutes
case_formatMinutes_5 =
  T.formatMinutes (T.Minutes 5) @?= "05"

-- Some interesting properties to check

{-|
  Converting from seconds to a time string and back should always
  succeed, and give back just the original seconds.

  Limit the range of natural numbers for the test.
-}
prop_round_trip_seconds_to_seconds =
  forAll (choose (0, 3000000)) $
  \seconds ->
  T.timeToSeconds (T.secondsToTime seconds) == Just seconds

{-
  Test hours just up to 2000
-}
instance Arbitrary T.Hours where
  arbitrary = liftM T.Hours $ choose (0, 2000)

instance Arbitrary T.Minutes where
  arbitrary = liftM T.Minutes $ choose (0, 59)

instance Arbitrary T.Seconds where
  arbitrary = liftM T.Seconds $ choose (0, 59)

{-|
  Converting from a time string to seconds successfully should
  convert back to the original time string.

  Test with some legal time strings.
-}
prop_round_trip_time_to_time =
  forAll (do
            h <- arbitrary
            m <- arbitrary
            s <- arbitrary
            return $ T.formatHours h ++
                     ":" ++ T.formatMinutes m ++
                     ":" ++ T.formatSeconds s
         ) $
  \timeString ->
  case T.timeToSeconds timeString of
    Nothing           -> False
    Just totalSeconds -> T.secondsToTime totalSeconds == timeString
