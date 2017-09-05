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

-- | A time difference represented with y/m/d + h/m/s/ns
data Delta = Delta
  { dPeriod   :: Period   -- ^ An amount of conceptual calendar time in terms of years, months and days.
  , dDuration :: Duration -- ^ An amount of time measured in hours/mins/secs/nsecs
  }

-- | A time period between two Datetimes
data Interval = Interval
  { iStart :: Datetime
  , iStop  :: Datetime
  }

-- | Conversion function between Data.Hourglass.DateTime and Datetime defined in
-- this module.
--
-- This should be the only way to construct a Datetime value, given the use of
-- the partial toEnum function in the Datetime -> DateTime conversion functions
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

-- | Conversion function between Datetime and Data.Hourglass.DateTime
convert' :: Datetime -> DateTime
convert' dt = DateTime {
      dtDate = dtDate'
    , dtTime = dtTime'
    }
  where
    dtDate' = Date {
        dateYear  = year dt
      , dateMonth = toEnum (month dt)
      , dateDay   = day dt
      }

    dtTime' = TimeOfDay {
        todHour = fromIntegral (hour dt)
      , todMin  = fromIntegral (minute dt)
      , todSec  = fromIntegral (second dt)
      , todNSec = 0
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

-- | Add a delta to a date
add :: Datetime -> Delta -> Datetime
add dt (Delta period duration) =
    convert $ DateTime (dateAddPeriod d period) tod
  where
    -- Data.Hourglass.DateTime with duration added
    dt'@(DateTime d tod) = timeAdd (convert' dt) duration

-- | Subtract a delta from a date (Delta should be positive)
sub :: Datetime -> Delta -> Datetime
sub dt (Delta period duration) =
  add dt $ Delta (negatePeriod period) (negateDuration duration)

-- | Get the difference between two dates
diff :: Datetime -> Datetime -> Delta
diff d1 d2 = undefined
  where
    diffSecs = timeDiff (convert' d1) (convert' d2)

-- | Check whether a date lies within an interval
within :: Datetime -> Interval -> Bool
within dt (Interval start stop) =
    startDate <= origDate && origDate <= endDate
  where
    origDate  = convert' dt
    startDate = convert' start
    endDate   = convert' stop

-- | Get the difference (in days) between two dates
daysBetween :: Datetime -> Datetime -> Delta
daysBetween d1'' d2'' =
    Delta (Period 0 0 durDays) mempty
  where
    d1' = convert' d1''
    d2' = convert' d2''

    (d1,d2)  = if d1' <= d2' then (d1',d2') else (d2',d1')
    duration = fst $ fromSeconds $ timeDiff d1 d2
    durDays  = let (Hours hrs) = durationHours duration in fromIntegral hrs `div` 24

-- | Get the date of the first day in a month of a given year
fomonth :: Int -> Month -> Datetime
fomonth y m = convert $ DateTime (Date y m 1) (TimeOfDay 0 0 0 0)

-- | Get the date of the last day in a month of a given year
eomonth :: Int -> Month -> Datetime
eomonth y m = sub foNextMonth $ Delta (Period 0 0 1) mempty
  where
    nextMonth
      | fromEnum m == 11 = January
      | otherwise = toEnum $ fromEnum m + 1

    foNextMonth = convert $ DateTime (Date y nextMonth 1) (TimeOfDay 0 0 0 0)

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

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

negatePeriod :: Period -> Period
negatePeriod (Period y m d) = Period (-y) (-m) (-d)

negateDuration :: Duration -> Duration
negateDuration (Duration h m s ns) = Duration (-h) (-m) (-s) (-ns)
