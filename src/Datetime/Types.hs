{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module Datetime.Types (
  Datetime(..),
  Delta(..),
  Interval(..),

  -- ** Constructors
  secs,
  mins,
  hours,

  -- ** Deltas
  days,
  months,
  years,
  weeks,

  -- ** Ordering
  before,
  after,

  -- ** Ranges
  from,
  between,

  -- ** Validation
  validateDatetime,

  -- ** End-of-month
  eomonth,
  fomonth,

  -- ** Conversion
  dateTimeToDatetime,
  posixToDatetime,

  -- ** Timezones -- XXX Explicit sum type
  TimezoneOffset(..),
  toUTC,
  alterTimezone,

  -- ** Delta operation
  add,
  sub,
  diff,
  within,

  -- ** Fiscal Quarters
  fiscalQuarters,
  q1,
  q2,
  q3,
  q4,

  -- ** Time Parse
  parseDatetime,
  formatDatetime,

  -- ** System time
  now,
) where

import Protolude hiding (get, put, second, diff, from)

import Data.Aeson
import Data.Hourglass
import Data.Monoid ((<>))
import Data.Serialize

import Control.Monad (fail)
import GHC.Generics (Generic)

import Time.System (timezoneCurrent, dateCurrent)

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
  } deriving (Show, Eq, Generic, Hashable, ToJSON, FromJSON)

instance Serialize Datetime where
  put Datetime {..} = do
    putInt year
    putInt month
    putInt day
    putInt hour
    putInt minute
    putInt second
    putInt zone
    putInt week_day
    where
      putInt :: Int -> PutM ()
      putInt i = putInt64be (fromIntegral i :: Int64)

  get = do
    year     <- getInt
    month    <- getInt
    day      <- getInt
    hour     <- getInt
    minute   <- getInt
    second   <- getInt
    zone     <- getInt
    week_day <- getInt
    let dt = Datetime {..}
    case validateDatetime dt of
      Left err -> fail err
      Right _  -> pure dt
    where
      getInt :: Get Int
      getInt = fmap fromIntegral (get :: Get Int64)

-- | Check whether a date is correctly formed
validateDatetime :: Datetime -> Either [Char] ()
validateDatetime (Datetime {..}) = sequence_ [
    cond (year > 0)                       "Year is invalid"
  , cond (year < 3000)                    "Year is not in current millenium"
  , cond (month >= 1 && month <= 12)      "Month range is invalid"
  , cond (day >= 1 && day <= 31)          "Day range is invalid"
  , cond (hour >= 0 && hour <= 23)        "Hour range is invalid"
  , cond (minute >= 0 && minute <= 59)    "Minute range is invalid"
  , cond (second >= 0 && second <= 59)    "Second range is invalid"
  , cond (week_day >= 0 && week_day <= 6) "Week day range is invalid"
  ]
  where
    cond True msg = Right ()
    cond False msg = Left msg

-- | Convert a Datetime to UTC
toUTC :: Datetime -> Datetime
toUTC = alterTimezone timezone_UTC

-- | Alter the Datetime timezone using logic from Data.Hourglass
alterTimezone :: TimezoneOffset -> Datetime -> Datetime
alterTimezone tz dt' = dateTimeToDatetime tz dt
  where
    dt = localTimeUnwrap $ localTime (TimezoneOffset $ zone dt') (datetimeToDateTime dt')

-------------------------------------------------------------------------------
-- Deltas and Intervals
-------------------------------------------------------------------------------

-- | A time difference represented with Period (y/m/d) + Duration (h/m/s/ns)
-- where Duration represents the time diff < 24 hours.
data Delta = Delta
  { dPeriod   :: Period   -- ^ An amount of conceptual calendar time in terms of years, months and days.
  , dDuration :: Duration -- ^ An amount of time measured in hours/mins/secs/nsecs
  } deriving (Show, Generic)

instance Monoid Delta where
  mempty = Delta mempty mempty
  mappend (Delta p1 d1) (Delta p2 d2) =
    Delta (p1 `mappend` p2) (d1 `mappend` d2)

-- | A time period between two Datetimes
data Interval = Interval
  { iStart :: Datetime
  , iStop  :: Datetime
  } deriving (Eq, Show, Generic)

-- | Conversion function between Data.Hourglass.DateTime and Datetime defined in
-- this module.
--
-- This should be the only way to construct a Datetime value, given the use of
-- the partial toEnum function in the Datetime -> DateTime conversion functions
dateTimeToDatetime :: TimezoneOffset -> DateTime -> Datetime
dateTimeToDatetime tzo@(TimezoneOffset tzOffset) dt' = datetime
  where
    -- Convert DateTime to local time and then unwrap for conversion
    dt = localTimeUnwrap $ localTimeSetTimezone tzo $ localTimeFromGlobal dt'

    -- Build a Datetime from the localtime adjusted DateTime
    datetime = Datetime {
        year     = dateYear (dtDate dt)
      , month    = 1 + fromEnum (dateMonth (dtDate dt)) -- human convention starts at 1
      , day      = dateDay (dtDate dt)
      , hour     = fromIntegral $ todHour (dtTime dt)
      , minute   = fromIntegral $ todMin (dtTime dt)
      , second   = fromIntegral $ todSec (dtTime dt)
      , zone     = tzOffset
      , week_day = fromEnum $ getWeekDay (dtDate dt) -- Sunday is 0
      }

-- | Conversion function between Datetime and Data.Hourglass.DateTime
-- WARNING: Resulting DateTime value is offset UTC by the timezone, but the specific timezone offset is lost
datetimeToDateTime :: Datetime -> DateTime
datetimeToDateTime dt = DateTime {
      dtDate = dtDate'
    , dtTime = dtTime'
    }
  where
    dtDate' = Date {
        dateYear  = year dt
      , dateMonth = toEnum (-1 + month dt)
      , dateDay   = day dt
      }

    dtTime' = TimeOfDay {
        todHour = fromIntegral (hour dt)
      , todMin  = fromIntegral (minute dt)
      , todSec  = fromIntegral (second dt)
      , todNSec = 0
      }

posixToDatetime :: Int64 -> Datetime
posixToDatetime = dateTimeToDatetime timezone_UTC . timeFromElapsed . Elapsed . Seconds

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

-- | Infinite list of days starting from a single date.
from :: Datetime -> [Datetime]
from = iterate (flip add (days 1))

-- | List of days between two points
-- Warning: Converts second Datetime to same timezone as the start
between :: Datetime -> Datetime -> [Datetime]
between start end = takeWhile (before end { zone = zone start}) (from start)

timezoneOffsetDelta :: TimezoneOffset -> Delta
timezoneOffsetDelta (TimezoneOffset minutes') =
    days dys <> hours hrs <> mins minutes
  where
    (hrs',minutes) = minutes' `divMod` 60
    (dys,hrs) = hrs' `divMod` 24

-------------------------------------------------------------------------------
-- Ordering
-------------------------------------------------------------------------------

deriving instance Ord Datetime

compareDate :: Datetime -> Datetime -> Ordering
compareDate d1 d2 = compare (toUTC d1) (toUTC d2)

-- | Check if first date occurs before a given date
before :: Datetime -> Datetime -> Bool
before x y = (compareDate x y) == GT

-- | Check if first date occurs after a given date
after :: Datetime -> Datetime -> Bool
after x y  = (compareDate x y) == LT

-------------------------------------------------------------------------------
-- Calendar Arithmetic
-------------------------------------------------------------------------------

-- | Add a delta to a date
add :: Datetime -> Delta -> Datetime
add dt (Delta period duration) =
    dateTimeToDatetime tz $ DateTime (dateAddPeriod d period) tod
  where
    -- Data.Hourglass.DateTime with duration added
    (DateTime d tod) = timeAdd (datetimeToDateTime dt) duration
    tz = TimezoneOffset $ zone dt

-- | Subtract a delta from a date (Delta should be positive)
sub :: Datetime -> Delta -> Datetime
sub dt (Delta period duration) =
  add dt $ Delta (negatePeriod period) (negateDuration duration)

-- | Get the difference between two dates
-- Warning: this function expects both datetimes to be in the same timezone
diff :: Datetime -> Datetime -> Delta
diff d1' d2' = Delta period duration
  where
    (d1, d2) = dateTimeToDatetimeAndOrderDateTime (toUTC d1') (toUTC d2')

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
        dtpYrs = dateTimeAddPeriod dt pYrs
        dtpMos = dateTimeAddPeriod dt pMos
        dtpDys = dateTimeAddPeriod dt pDys

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
        d1dHrs = timeAdd dt dHrs
        d1dMns = timeAdd dt dMns
        d1dScs = timeAdd dt dScs

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
    Delta (Period 0 0 (abs durDays)) mempty
  where
    (d1,d2)  = dateTimeToDatetimeAndOrderDateTime (toUTC d1') (toUTC d2')
    duration = fst $ fromSeconds $ timeDiff d1 d2
    durDays  = let (Hours hrs) = durationHours duration in fromIntegral hrs `div` 24

-- | Get the date of the first day in a month of a given year
fomonth :: Int -> Month -> Datetime
fomonth y m = dateTimeToDatetime timezone_UTC $
  DateTime (Date y m 1) (TimeOfDay 0 0 0 0)

-- | Get the date of the last day in a month of a given year
eomonth :: Int -> Month -> Datetime
eomonth y m = sub foNextMonth $ Delta (Period 0 0 1) mempty
  where
    (year,nextMonth) -- if next month is January, inc year (will be dec in `sub` above)
      | fromEnum m == 11 = (y+1, January)
      | otherwise = (y, toEnum $ fromEnum m + 1)

    foNextMonth = dateTimeToDatetime timezone_UTC $
      DateTime (Date year nextMonth 1) (TimeOfDay 0 0 0 0)

-------------------------------------------------------------------------------
-- Fiscal Quarters
-------------------------------------------------------------------------------

fiscalQuarters :: Int -> (Interval, Interval, Interval, Interval)
fiscalQuarters year = (q1 year, q2 year, q3 year, q4 year)

q1, q2, q3, q4 :: Int -> Interval
q1 year = Interval (fomonth year January) (eomonth year March)
q2 year = Interval (fomonth year April) (eomonth year June)
q3 year = Interval (fomonth year July) (eomonth year September)
q4 year = Interval (fomonth year January) (eomonth year March)

-------------------------------------------------------------------------------
-- Datetime Parsing
-------------------------------------------------------------------------------

-- | Parses either an ISO8601 DateAndTime string: "2014-04-05T17:25:04+05:00"
parseDatetime :: [Char] -> Maybe Datetime
parseDatetime timestr = do
  localTime <- localTimeParse ISO8601_DateAndTime timestr
  let dateTime = localTimeUnwrap localTime
  let tzOffset = localTimeGetTimezone localTime
  pure $ dateTimeToDatetime tzOffset dateTime

formatDatetime :: Datetime -> [Char]
formatDatetime = (timePrint ISO8601_DateAndTime) . datetimeToDateTime

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
minMax mini maxi = max mini . min maxi

-------------------------------------------------------------------------------
-- System Time
-------------------------------------------------------------------------------

-- | Current system time in UTC
now :: IO Datetime
now = dateTimeToDatetime timezone_UTC <$> dateCurrent

-- | Current system time in Local time
localNow :: IO Datetime
localNow = do
  tz <- timezoneCurrent
  alterTimezone tz <$> now
