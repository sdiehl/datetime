{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Monoid
import Data.Hourglass

import Time.System

data Datetime = Datetime
  { year     :: Int -- the complete year
  , month    :: Int -- a month, between 1 and 12
  , day      :: Int -- a day, between 1 and 31
  , hour     :: Int -- the number of hours since midnight, between 0 and 23
  , minute   :: Int -- the number of minutes since the beginning of the hour, between 0 and 59
  , second   :: Int -- the number of seconds since the begining of the minute, between 0 and 59
  , zone     :: Int -- the local zone offset, in minutes of advance wrt UTC.
  , week_day :: Int -- the number of days since sunday, between 0 and 6
  } deriving (Show)

data Delta = Delta
  { dPeriod   :: Period
  , dDuration :: Duration
  }

data Interval = Interval
  { istart :: Datetime
  , iStop  :: Datetime
  }

convert :: DateTime -> Datetime
convert dt = Datetime {
    year     = dateYear (dtDate dt)
  , month    = fromEnum (dateMonth (dtDate dt))
  , day      = dateDay (dtDate dt)
  , hour     = fromIntegral $ todHour (dtTime dt)
  , minute   = fromIntegral $ todMin (dtTime dt)
  , second   = fromIntegral $ todSec (dtTime dt)
  , zone     = 0
  , week_day = fromEnum $ getWeekDay (dtDate dt)
  }

days :: Int -> Period
days n = Period { periodYears = 0, periodMonths = 0, periodDays = n }

months :: Int -> Period
months n = Period { periodYears = 0, periodMonths = n, periodDays = 0 }

years :: Int -> Period
years n = Period { periodYears = n, periodMonths = 0, periodDays = 0 }

weeks :: Int -> Period
weeks n = days (7*n)

fortnights :: Int -> Period
fortnights n = weeks (2*n)

composite :: Period
composite = days 3 <> months 4 <> years 1

isHoliday :: Date -> Bool
isHoliday _ = False

isWeekday :: Date -> Bool
isWeekday dt = go (getWeekDay dt)
  where
    go = \case
      Saturday -> False
      Sunday   -> False
      _        -> True

isWeekend :: Date -> Bool
isWeekend = not . isWeekday

isBusiness :: Date -> Bool
isBusiness dt = not (isHoliday dt) && not (isWeekend dt)

-------------------------------------------------------------------------------
-- Testing
-------------------------------------------------------------------------------

nowTz :: IO TimezoneOffset
nowTz = timezoneCurrent

nowDate :: IO DateTime
nowDate = dateCurrent

example :: IO Date
example = do
  dt <- nowDate
  pure (dateAddPeriod (dtDate dt) composite)

main :: IO ()
main = return ()
