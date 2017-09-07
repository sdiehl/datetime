{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Holiday.Types (
  Datetime(..),
  Delta,
  dPeriod,
  dDuration,

  secs,
  mins,
  hours,

  days,
  months,
  years,
  weeks,

  eomonth,
  fomonth,

  dateTimeToDatetime,
  datetimeToDateTime,

  add,
  sub,
  diff,
  within,
) where

import Data.Hourglass

import Data.Aeson
import GHC.Generics (Generic)
import Data.Monoid ((<>))
import Data.Serialize

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------


data Datetime = Datetime
  { year     :: Int -- ^ The complete year
  , month    :: Int -- ^ A month, between 1 and 12
  , day      :: Int -- ^ A day, between 1 and 31
  , hour     :: Int -- ^ The number of hours since midnight, between 0 and 23
  , minute   :: Int -- ^ The number of minutes since the beginning of the hour, between 0 and 59
  , second   :: Int -- ^ The number of seconds since the begining of the minute, between 0 and 59
  , zone     :: Int -- ^ The local zone offset, in minutes of advance wrt UTC.
  , week_day :: Int -- ^ The number of days since sunday, between 0 and 6
  } deriving (Show, Generic, ToJSON, FromJSON, Serialize)

-------------------------------------------------------------------------------
-- Deltas and Intervals
-------------------------------------------------------------------------------

-- | A time difference represented with Period (y/m/d) + Duration (h/m/s/ns)
-- where Duration represents the time diff < 24 hours.
data Delta = Delta
  { dPeriod   :: Period   -- ^ An amount of conceptual calendar time in terms of years, months and days.
  , dDuration :: Duration -- ^ An amount of time measured in hours/mins/secs/nsecs
  } deriving (Show)

instance Monoid Delta where
  mempty = Delta mempty mempty
  mappend (Delta p1 d1) (Delta p2 d2) =
    Delta (p1 `mappend` p2) (d1 `mappend` d2)

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
dateTimeToDatetime :: DateTime -> Datetime
dateTimeToDatetime dt = Datetime {
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
datetimeToDateTime :: Datetime -> DateTime
datetimeToDateTime dt = DateTime {
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

-------------------------------------------------------------------------------
-- Delta combinators
-------------------------------------------------------------------------------

-- | Trimmed to 0 - 59
secs :: Int -> Delta
secs n = Delta mempty $ Duration 0 0 (fromIntegral $ minMax 0 59 n) 0

-- | Trimmed to 0 - 59
mins :: Int -> Delta
mins n = Delta mempty $ Duration 0 (fromIntegral $ minMax 0 59 n) 0 0

-- | Trimmed to 0 - 23
hours :: Int -> Delta
hours n = Delta mempty $ Duration (fromIntegral $ minMax 0 23 n) 0 0 0

days :: Int -> Delta
days n = flip Delta mempty
  Period { periodYears = 0, periodMonths = 0, periodDays = n }

months :: Int -> Delta
months n = flip Delta mempty
  Period { periodYears = 0, periodMonths = n, periodDays = 0 }

years :: Int -> Delta
years n = flip Delta mempty
  Period { periodYears = n, periodMonths = 0, periodDays = 0 }

weeks :: Int -> Delta
weeks n = days (7*n)

fortnights :: Int -> Delta
fortnights n = weeks (2*n)

-------------------------------------------------------------------------------
-- Calendar Arithmetic
-------------------------------------------------------------------------------

-- | Add a delta to a date
add :: Datetime -> Delta -> Datetime
add dt (Delta period duration) =
    dateTimeToDatetime $ DateTime (dateAddPeriod d period) tod
  where
    -- Data.Hourglass.DateTime with duration added
    dt'@(DateTime d tod) = timeAdd (datetimeToDateTime dt) duration

-- | Subtract a delta from a date (Delta should be positive)
sub :: Datetime -> Delta -> Datetime
sub dt (Delta period duration) =
  add dt $ Delta (negatePeriod period) (negateDuration duration)

-- | Get the difference between two dates (always positive (?))
diff :: Datetime -> Datetime -> Delta
diff d1' d2' = Delta period duration
  where
    (d1, d2) = dateTimeToDatetimeAndOrderDateTime d1' d2'

    period = buildPeriodDiff d1 mempty
    d1PlusPeriod = dateTimeAddPeriod d1 period
    duration = buildDurDiff d1PlusPeriod mempty

    -- Build the period part of the Delta
    buildPeriodDiff :: DateTime -> Period -> Period
    buildPeriodDiff dt p
      | dtpYrs <= d2 = buildPeriodDiff dt pYrs
      | dtpMos <= d2 = buildPeriodDiff dt pMos
      | dtpDys <= d2 = buildPeriodDiff dt pDys
      | otherwise    = p
      where
        pYrs = p <> dPeriod (years 1)
        pMos = p <> dPeriod (months 1)
        pDys = p <> dPeriod (days 1)
        [dtpYrs, dtpMos, dtpDys] = flip map [pYrs,pMos,pDys] $ dateTimeAddPeriod dt

    -- Build the duration part of the delta
    buildDurDiff :: DateTime -> Duration -> Duration
    buildDurDiff dt d
      | d1dHrs <= d2 = buildDurDiff dt dHrs
      | d1dMns <= d2 = buildDurDiff dt dMns
      | d1dScs <= d2 = buildDurDiff dt dScs
      | otherwise    = d
      where
        dHrs = d { durationHours = durationHours d + 1 }
        dMns = d { durationMinutes = durationMinutes d + 1 }
        dScs = d { durationSeconds = durationSeconds d + 1 }
        [d1dHrs, d1dMns, d1dScs] = map (timeAdd dt) [dHrs,dMns,dScs]

-- | Check whether a date lies within an interval
within :: Datetime -> Interval -> Bool
within dt (Interval start stop) =
    startDate <= origDate && origDate <= endDate
  where
    origDate  = datetimeToDateTime dt
    startDate = datetimeToDateTime start
    endDate   = datetimeToDateTime stop

-- | Get the difference (in days) between two dates
daysBetween :: Datetime -> Datetime -> Delta
daysBetween d1' d2' =
    Delta (Period 0 0 durDays) mempty
  where
    (d1,d2)  = dateTimeToDatetimeAndOrderDateTime d1' d2'
    duration = fst $ fromSeconds $ timeDiff d1 d2
    durDays  = let (Hours hrs) = durationHours duration in fromIntegral hrs `div` 24

-- | Get the date of the first day in a month of a given year
fomonth :: Int -> Month -> Datetime
fomonth y m = dateTimeToDatetime $ DateTime (Date y m 1) (TimeOfDay 0 0 0 0)

-- | Get the date of the last day in a month of a given year
eomonth :: Int -> Month -> Datetime
eomonth y m = sub foNextMonth $ Delta (Period 0 0 1) mempty
  where
    nextMonth
      | fromEnum m == 11 = January
      | otherwise = toEnum $ fromEnum m + 1

    foNextMonth = dateTimeToDatetime $ DateTime (Date y nextMonth 1) (TimeOfDay 0 0 0 0)

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

negatePeriod :: Period -> Period
negatePeriod (Period y m d) = Period (-y) (-m) (-d)

negateDuration :: Duration -> Duration
negateDuration (Duration h m s ns) = Duration (-h) (-m) (-s) (-ns)

dateTimeAddPeriod :: DateTime -> Period -> DateTime
dateTimeAddPeriod (DateTime ddate dtime) p =
  DateTime (dateAddPeriod ddate p) dtime

dateTimeToDatetimeAndOrderDateTime :: Datetime -> Datetime -> (DateTime, DateTime)
dateTimeToDatetimeAndOrderDateTime d1' d2'
  | d1 <= d2  = (d1,d2)
  | otherwise = (d2,d1)
  where
    d1 = datetimeToDateTime d1'
    d2 = datetimeToDateTime d2'

minMax :: Ord a => a -> a -> a -> a
minMax mini maxi = max maxi . min mini
