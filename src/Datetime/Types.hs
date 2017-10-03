{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Datetime.Types (
  Datetime(..),
  Delta(..),
  Period(..),
  Duration(..),
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
  datetimeToDateTime,
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

  displayDelta,

  -- ** System time
  now,
) where

import Protolude hiding (get, put, second, diff, from)

import Control.Monad (fail)

import Data.Aeson

import Data.Hourglass
  ( DateTime(..)
  , Date(..)
  , Month(..)
  , TimeOfDay(..)
  , TimezoneOffset(..)

  , LocalTime(..)
  , localTime
  , localTimeParse
  , localTimeUnwrap
  , localTimeUnwrap
  , localTimeSetTimezone
  , localTimeFromGlobal
  , localTimeToGlobal
  , ISO8601_DateAndTime(..)
  )

import qualified Data.Hourglass as DH

import Data.Monoid ((<>))
import Data.Serialize

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
  } deriving (Show, Eq, Generic, NFData, Hashable, ToJSON, FromJSON)

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
toUTC = alterTimezone DH.timezone_UTC

-- | Alter the Datetime timezone using logic from Data.Hourglass
alterTimezone :: TimezoneOffset -> Datetime -> Datetime
alterTimezone tz = dateTimeToDatetime tz . datetimeToDateTime

-------------------------------------------------------------------------------
-- Deltas and Intervals
-------------------------------------------------------------------------------

newtype Period = Period { unPeriod :: DH.Period }
  deriving (Show, Eq, Ord, Generic, NFData)

instance Monoid Period where
  mempty = Period mempty
  mappend (Period p1) (Period p2) = Period $ mappend p1 p2

instance Hashable Period where
  hashWithSalt salt (Period (DH.Period yrs mns dys)) =
    foldl' hashWithSalt salt [yrs,mns,dys]

instance ToJSON Period where
  toJSON (Period (DH.Period yrs mns dys)) = object
    [ "periodYears"  .= yrs
    , "periodMonths" .= mns
    , "periodDays"   .= dys
    ]

instance FromJSON Period where
  parseJSON = withObject "Period" $ \v ->
    fmap Period $ DH.Period
      <$> v .: "periodYears"
      <*> v .: "periodMonths"
      <*> v .: "periodDays"

instance Serialize Period where
  put (Period (DH.Period yrs mns dys)) = do
      putInt yrs
      putInt mns
      putInt dys
    where
      putInt = putInt64be . fromIntegral
  get = fmap Period $
    DH.Period
      <$> getInt
      <*> getInt
      <*> getInt
    where
      getInt = fromIntegral <$> getInt64be

newtype Duration = Duration { unDuration :: DH.Duration }
  deriving (Show, Eq, Ord, Generic, NFData)

instance Monoid Duration where
  mempty = Duration mempty
  mappend (Duration d1) (Duration d2) = Duration $ mappend d1 d2

instance Hashable Duration where
  hashWithSalt salt (Duration (DH.Duration (DH.Hours h) (DH.Minutes m) (DH.Seconds s) (DH.NanoSeconds ns))) =
    foldl' hashWithSalt salt $ map (fromIntegral :: Int64 -> Int) [h,m,s,ns]

instance ToJSON Duration where
  toJSON (Duration duration) =
    let (DH.Duration (DH.Hours h) (DH.Minutes m) (DH.Seconds s) (DH.NanoSeconds ns)) = duration
    in object [ "durationHours"   .= h
              , "durationMinutes" .= m
              , "durationSeconds" .= s
              , "durationNs"      .= ns
              ]

instance FromJSON Duration where
  parseJSON = withObject "Duration" $ \v ->
    fmap Duration $ DH.Duration
      <$> (fmap DH.Hours       $ v .: "durationHours")
      <*> (fmap DH.Minutes     $ v .: "durationMinutes")
      <*> (fmap DH.Seconds     $ v .: "durationSeconds")
      <*> (fmap DH.NanoSeconds $ v .: "durationNs")

instance Serialize Duration where
  put (Duration duration) = do
    let (DH.Duration (DH.Hours h) (DH.Minutes m) (DH.Seconds s) (DH.NanoSeconds ns)) = duration
    putInt64be h
    putInt64be m
    putInt64be s
    putInt64be ns
  get = fmap Duration $
    DH.Duration
      <$> (fmap DH.Hours getInt64be)
      <*> (fmap DH.Minutes getInt64be)
      <*> (fmap DH.Seconds getInt64be)
      <*> (fmap DH.NanoSeconds getInt64be)

-- | A time difference represented with Period (y/m/d) + Duration (h/m/s/ns)
-- where Duration represents the time diff < 24 hours.
data Delta = Delta
  { dPeriod   :: Period   -- ^ An amount of conceptual calendar time in terms of years, months and days.
  , dDuration :: Duration -- ^ An amount of time measured in hours/mins/secs/nsecs
  } deriving (Show, Eq, Ord, Generic, NFData, Hashable, Serialize, ToJSON, FromJSON)

displayDelta :: Delta -> Text
displayDelta (Delta (Period (DH.Period y mo dy)) (Duration d)) =
    year <> month <> day <> hour <> minute <> second
  where
    (DH.Duration (DH.Hours h) (DH.Minutes m) (DH.Seconds s) (DH.NanoSeconds ns)) = d

    year  = suffix y  "y"
    month = suffix mo "mo"
    day   = suffix dy "d"

    hour   = suffix h "h"
    minute = suffix m "m"
    second = suffix s "s"

    suffix :: (Eq a, Num a, Show a) => a -> Text -> Text
    suffix n s
      | n == 0 = ""
      | otherwise = show n <> s

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
      , week_day = fromEnum $ DH.getWeekDay (dtDate dt) -- Sunday is 0
      }

-- | Conversion function between Datetime and Data.Hourglass.DateTime
-- WARNING: Resulting DateTime value is offset UTC by the timezone,
-- but data about the specific timezone offset is lost
datetimeToDateTime :: Datetime -> DateTime
datetimeToDateTime dt =
    localTimeToGlobal $
      localTime (TimezoneOffset $ zone dt) $
        DateTime { dtDate = dtDate'
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
posixToDatetime = dateTimeToDatetime DH.timezone_UTC . DH.timeFromElapsed . DH.Elapsed . DH.Seconds

-------------------------------------------------------------------------------
-- Delta combinators
-------------------------------------------------------------------------------

-- | Trimmed to 0 - 59
secs :: Int -> Delta
secs n = Delta mempty $ Duration $
  DH.Duration 0 0 (fromIntegral $ minMax 0 59 n) 0

-- | Trimmed to 0 - 59
mins :: Int -> Delta
mins n = Delta mempty $ Duration $
  DH.Duration 0 (fromIntegral $ minMax 0 59 n) 0 0

-- | Trimmed to 0 - 23
hours :: Int -> Delta
hours n = Delta mempty $ Duration $
  DH.Duration (fromIntegral $ minMax 0 23 n) 0 0 0

days :: Int -> Delta
days n = flip Delta mempty $ Period $
  DH.Period { DH.periodYears = 0, DH.periodMonths = 0, DH.periodDays = n }

months :: Int -> Delta
months n = flip Delta mempty $ Period
  DH.Period { DH.periodYears = 0, DH.periodMonths = n, DH.periodDays = 0 }

years :: Int -> Delta
years n = flip Delta mempty $ Period
  DH.Period { DH.periodYears = n, DH.periodMonths = 0, DH.periodDays = 0 }

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

instance Ord Datetime where
  d1 `compare` d2 = (datetimeToDateTime $ toUTC d1) `compare` (datetimeToDateTime $ toUTC d2)

-- | Check if first date occurs before a given date
before :: Datetime -> Datetime -> Bool
before = (<)

-- | Check if first date occurs after a given date
after :: Datetime -> Datetime -> Bool
after = (>)

-------------------------------------------------------------------------------
-- Calendar Arithmetic
-------------------------------------------------------------------------------

-- | Add a delta to a date
add :: Datetime -> Delta -> Datetime
add dt (Delta (Period period) (Duration duration)) =
    dateTimeToDatetime tz $ DateTime (DH.dateAddPeriod d period) tod
  where
    -- Data.Hourglass.DateTime with duration added
    (DateTime d tod) = DH.timeAdd (datetimeToDateTime dt) duration
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
    d1PlusPeriod = dateTimeAddPeriod d1 $ unPeriod period
    duration = buildDurDiff d1PlusPeriod mempty

    -- Build the period part of the Delta
    buildPeriodDiff :: DH.DateTime -> DH.Period -> Period
    buildPeriodDiff dt p
      | dtpYrs <= d2 = buildPeriodDiff dt pYrs
      | dtpMos <= d2 = buildPeriodDiff dt pMos
      | dtpDys <= d2 = buildPeriodDiff dt pDys
      | otherwise    = Period p
      where
        (Period pYrs) = Period p <> dPeriod (years 1)
        (Period pMos) = Period p <> dPeriod (months 1)
        (Period pDys) = Period p <> dPeriod (days 1)
        dtpYrs = dateTimeAddPeriod dt pYrs
        dtpMos = dateTimeAddPeriod dt pMos
        dtpDys = dateTimeAddPeriod dt pDys

    -- Build the duration part of the delta
    buildDurDiff :: DH.DateTime -> DH.Duration -> Duration
    buildDurDiff dt d
      | d1dHrs <= d2 = buildDurDiff dt dHrs
      | d1dMns <= d2 = buildDurDiff dt dMns
      | d1dScs <= d2 = buildDurDiff dt dScs
      | otherwise    = Duration d
      where
        dHrs = d { DH.durationHours = DH.durationHours d + 1 }
        dMns = d { DH.durationMinutes = DH.durationMinutes d + 1 }
        dScs = d { DH.durationSeconds = DH.durationSeconds d + 1 }
        d1dHrs = DH.timeAdd dt dHrs
        d1dMns = DH.timeAdd dt dMns
        d1dScs = DH.timeAdd dt dScs

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
    Delta (Period $ DH.Period 0 0 (abs durDays)) mempty
  where
    (d1,d2)  = dateTimeToDatetimeAndOrderDateTime (toUTC d1') (toUTC d2')
    duration = fst $ DH.fromSeconds $ DH.timeDiff d1 d2

    durDays  = let (DH.Hours hrs) = DH.durationHours duration
               in fromIntegral hrs `div` 24

-- | Get the date of the first day in a month of a given year
fomonth :: Int -> DH.Month -> Datetime
fomonth y m = dateTimeToDatetime DH.timezone_UTC $
  DateTime (Date y m 1) (TimeOfDay 0 0 0 0)

-- | Get the date of the last day in a month of a given year
eomonth :: Int -> DH.Month -> Datetime
eomonth y m = sub foNextMonth $ Delta (Period $ DH.Period 0 0 1) mempty
  where
    (year,nextMonth) -- if next month is January, inc year (will be dec in `sub` above)
      | fromEnum m == 11 = (y+1, January)
      | otherwise = (y, toEnum $ fromEnum m + 1)

    foNextMonth = dateTimeToDatetime DH.timezone_UTC $
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
formatDatetime = DH.timePrint ISO8601_DateAndTime . datetimeToDateTime

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

negatePeriod :: Period -> Period
negatePeriod (Period (DH.Period y m d)) = Period (DH.Period (-y) (-m) (-d))

negateDuration :: Duration -> Duration
negateDuration (Duration (DH.Duration h m s ns)) = Duration (DH.Duration (-h) (-m) (-s) (-ns))

dateTimeAddPeriod :: DateTime -> DH.Period -> DateTime
dateTimeAddPeriod (DateTime ddate dtime) p =
  DateTime (DH.dateAddPeriod ddate p) dtime

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
now = dateTimeToDatetime DH.timezone_UTC <$> dateCurrent

-- | Current system time in Local time
localNow :: IO Datetime
localNow = do
  tz <- timezoneCurrent
  alterTimezone tz <$> now
