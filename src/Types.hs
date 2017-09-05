{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Types (
  Datetime,
  days,
  months,
  years,
  weeks,

  add,
  sub,
  diff,
  within,

  isWeekend,
  isWeekday,
  isBusiness,
) where

import Data.Hourglass

import Data.Aeson
import Data.Serialize

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------


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

instance ToJSON Datetime where
  toJSON = undefined -- XXX

instance Serialize Datetime where
  put = undefined -- XXX
  get = undefined -- XXX

-------------------------------------------------------------------------------
-- Deltas and Intervals
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- Calendar Arithmetic
-------------------------------------------------------------------------------

-- | Ad a period to a date
add :: Date -> Period -> Date
add = undefined  -- XXX

-- | Subtract a period from a date
sub :: Date -> Delta -> Date
sub = undefined  -- XXX

-- | Get the difference between two dates
diff :: Date -> Date -> Delta
diff = undefined -- XXX

-- | Check whether a date lies within an interval
within :: Date -> Interval -> Bool
within = undefined -- XXX

-- | Get the difference (in days) between two dates
daysBetween :: Date -> Date -> Delta
daysBetween = undefined -- XXX

-- | Get the date of the last day in a month
eomonth :: Month -> Date
eomonth = undefined -- XXX

-------------------------------------------------------------------------------
-- Queries
-------------------------------------------------------------------------------

isHoliday :: Date -> Bool
isHoliday _ = False -- XXX

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
