{-|
  Time to string conversions
-}
module Time1 where

import qualified Data.Char as Char
import qualified Data.List as List

{-|
  0 to 9
-}
newtype Digit = Digit Int
  deriving (Eq, Show)

type DigitString = [Digit]

digitsToInt :: DigitString -> Int
digitsToInt = List.foldl' (\acc (Digit d) -> 10 * acc + d) 0

newtype Hours = Hours Int

newHours h | h >= 0    = Just $ Hours h
           | otherwise = Nothing

newtype Minutes = Minutes Int

newMinutes m | m >= 0 && m < 60 = Just $ Minutes m
             | otherwise        = Nothing

newtype Seconds = Seconds Int

newSeconds s | s >= 0 && s < 60 = Just $ Seconds s
             | otherwise        = Nothing

{-|
  Convert time in seconds to string format
-}
secondsToTime :: Int    -- ^ @totalSeconds@ >= 0
              -> String -- ^ ...HHHH:MM:SS
secondsToTime totalSeconds =
  formatHours(hours) ++
    ":" ++ formatMinutes(minutes) ++
    ":" ++ formatSeconds(seconds)
  where (hours, minutes, seconds) = secondsToClock totalSeconds

{-|
  Precondition @totalSeconds@ >= 0

  Postconditions: @hours@ >= 0, 0 <= @minutes@ < 60, 0 <= @seconds@ < 60
-}
secondsToClock :: Int -> (Hours, Minutes, Seconds)
secondsToClock totalSeconds =
  (Hours hours, Minutes minutes, Seconds seconds)
  where hours = totalSeconds `div` (60 * 60)
        remainingSeconds = totalSeconds - hours * (60 * 60)
        minutes = remainingSeconds `div` 60
        seconds = remainingSeconds - minutes * 60

formatHours :: Hours -> String
formatHours (Hours h) = show h

{-|
  Leading 0 if necessary
-}
formatMinutes :: Minutes -> String
formatMinutes (Minutes m)
  | m < 10    = '0' : show m
  | otherwise = show m

{-|
  Leading 0 if necessary
-}
formatSeconds :: Seconds -> String
formatSeconds (Seconds s)
  | s < 10    = '0' : show s
  | otherwise = show s

{-|
  Convert time string to a time in seconds.

  For illustration, do this by hand without regex, parsing, or monads!

  Use two phases: parse colon-separated list of digits,
  then verify the constraints on that list.

  Make sure minutes and seconds are exactly two digits.
-}
timeToSeconds :: String    -- ^ should be in format ...HHHH:MM:SS
              -> Maybe Int -- ^ time in seconds, if successful parse
timeToSeconds s = 
  case splitDigitsAtColon s of
    Nothing -> Nothing
    Just [hoursString,
          minutesString @ [_, _],
          secondsString @ [_, _]] ->
      let
        hours = digitsToInt hoursString
        minutes = digitsToInt minutesString
        seconds = digitsToInt secondsString
      in
        if minutes >= 60 || seconds >= 60 then
          Nothing
        else
          Just $ hours * 60 * 60 + minutes * 60 + seconds
    _ -> Nothing

{-|
  Helper to split a string at colons on digits to get numerical strings.

  This should be in a standard library.
-}
splitDigitsAtColon :: String -> Maybe [DigitString]
splitDigitsAtColon xs = splitDigitsAtColon' xs []

{-|
  TODO: Could write monadically, but not now.
-}
splitDigitsAtColon' :: String
                    -> DigitString          -- ^ stack for current digit string
                    -> Maybe [DigitString] -- ^ string with only 0 through 9
splitDigitsAtColon' [] stack = Just [reverse stack]
splitDigitsAtColon' (':':xs) stack =
  case splitDigitsAtColon' xs [] of
    Nothing   -> Nothing
    Just stacks -> Just $ (reverse stack):stacks
splitDigitsAtColon' (x:xs) stack
  | x >= '0' && x <= '9' =
    splitDigitsAtColon' xs $ (Digit (Char.ord x - Char.ord '0')) : stack
splitDigitsAtColon' _ stack = Nothing
