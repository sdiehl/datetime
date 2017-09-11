{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Holiday (
  -- ** Holiday recurrence rules
  HolidaySet(..),
  HolidayRule(..),
  Observance(..),
  Holiday,
  WeekdayPos,

  -- ** Holiday sets
  HolidayGen,
  ukHolidays,
  nyseHolidays,

  -- ** Business day status
  isWeekend,
  isWeekday,
  isBusiness,
  isHoliday,

  -- ** Holiday queries
  isUKHoliday,
  isNYSEHoliday,
) where

import Protolude hiding (First, Last)

import Data.Hourglass
import Data.Time.Calendar (toGregorian)
import Data.Time.Calendar.Easter (gregorianEaster)

import Holiday.Types

data HolidaySet
  = UnitedKingdom [Holiday]
  | NYSE [Holiday]
  deriving (Show)

data Observance
  = Nearest_workday         -- ^ Move Saturday to Friday and Sunday to Monday
  | Sunday_to_monday        -- ^ Move Sunday to following Monday
  | Next_monday_or_tuesday  -- ^ Move Saturday to Monday and Sunday/Monday to Tuesday
  | Previous_friday         -- ^ Move Saturday and Sunday to previous Friday
  | Next_monday             -- ^ Move Saturday and Sunday to following Monday
  | None                    -- ^ Always on a fixed date
  deriving (Show, Eq)

data FixedHoliday = FixedHoliday
  { recurrenceDay   :: Int
  , recurrenceMonth :: Month
  , observance      :: Observance
  , timezone        :: TimezoneOffset
  } deriving (Show, Eq)

data WeekdayPos
  = First   -- ^ The first occurrence of the weekday
  | Second  -- ^ The second occurrence of the weekday
  | Third   -- ^ The third occurrence of the weekday
  | Fourth  -- ^ The fourth occurrence of the weekday
  | Last    -- ^ The fifth, or last occurrence. Sometimes overlaps with Fourth.
  deriving (Show, Eq, Enum)

-- | Specifies a recurrence rule for holidays that have a logical yearly
-- recurrence date rather than a fixed date every year. YEAR recurrence
-- is implicit, therefore no recurrence rule is specified.
data HolidayRule = HolidayRule
  { monthOfYear :: Month      -- ^ The month the holiday occurs on
  , weekDayPos  :: WeekdayPos -- ^ What number weekday in the month (e.g. 4th Monday)
  , weekDay     :: WeekDay    -- ^ The weekday the holiday occurs on
  } deriving (Show, Eq)

data EasterHoliday = EasterHoliday Datetime
  deriving (Show, Eq)

data Holiday
  = Fixed FixedHoliday
  | Rule HolidayRule
  | Easter EasterHoliday
  deriving (Show, Eq)

partitionHolidays :: [Holiday] -> ([FixedHoliday],[HolidayRule],[EasterHoliday])
partitionHolidays = foldl' partition' ([],[],[])
  where
    partition' (fs,rs,es) holiday =
      case holiday of
        Fixed fixed -> (fixed:fs,rs,es)
        Rule rule   -> (fs,rule:rs,es)
        Easter ehol -> (fs,rs,ehol:es)


observedShift :: Observance -> Datetime -> Datetime
observedShift obs datetime = case obs of

  Nearest_workday -> case getWeekDay date of
    Saturday -> sub datetime (days 1) -- prev friday
    Sunday   -> add datetime (days 1) -- next monday
    _        -> datetime

  Sunday_to_monday -> case getWeekDay date of
    Sunday   -> add datetime (days 1) -- next monday
    _        -> datetime

  Next_monday_or_tuesday -> case getWeekDay date of
    Saturday -> add datetime (days 2) -- next monday
    Sunday   -> add datetime (days 2) -- next tuesday
    Monday   -> add datetime (days 1) -- next tuesday
    _        -> datetime

  Previous_friday -> case getWeekDay date of
    Saturday -> sub datetime (days 1) -- previous friday
    Sunday   -> sub datetime (days 2) -- previous friday
    _        -> datetime

  Next_monday -> case getWeekDay date of
    Saturday -> add datetime (days 2) -- next monday
    Sunday   -> add datetime (days 1) -- next monday
    _        -> datetime

  None       -> datetime

  where
    dateTime = datetimeToDateTime datetime
    date = dtDate dateTime

-------------------------------------------------------------------------------
-- Queries
-------------------------------------------------------------------------------

type HolidayGen = Int -> [Holiday]

isHoliday :: HolidayGen -> Datetime -> Bool
isHoliday hs dt = matchHolidays dt holidays
  where
    holidays = hs (year dt)

isUKHoliday :: Datetime -> Bool
isUKHoliday dt = matchHolidays dt holidays
  where
    holidays = ukHolidays (year dt)

isNYSEHoliday :: Datetime -> Bool
isNYSEHoliday dt = matchHolidays dt holidays
  where
    holidays = nyseHolidays (year dt)

matchHolidays :: Datetime -> [Holiday] -> Bool
matchHolidays dt holidays = or
    [ any (matchFixedHoliday dt) fixeds
    , any (matchHolidayRule dt) rules
    , any (matchEasterHoliday dt) easters
    ]
  where
    (fixeds, rules, easters) = partitionHolidays holidays

-- | A Datetime matches a fixed holiday if the day/month/year is equal to the
-- day/month/year of the fixed holiday
matchFixedHoliday :: Datetime -> FixedHoliday -> Bool
matchFixedHoliday dt (FixedHoliday fday fmonth obs tz) = and
  [ month dt == month shiftedDt
  , day dt   == day shiftedDt
  -- XXX , match timezone here
  ]
  where
    fixedDate = Date (year dt) fmonth fday
    fixedDT   = DateTime fixedDate $ TimeOfDay 0 0 0 0
    shiftedDt = observedShift obs $ dateTimeToDatetime fixedDT

-- | A Datetime matches the Holiday rule iff:
-- 1) the weekday of the datetime is the same as in the holiday rule
-- 2) the month of the datetime is the same as in the holiday rule
-- 3) the position of the weekday in the month matches
matchHolidayRule :: Datetime -> HolidayRule -> Bool
matchHolidayRule dt (HolidayRule month' wkDayPos wkDay) = and
    [ getWeekDay date   == wkDay    -- 1)
    , dateMonth date    == month'   -- 2)
    , findWeekdayPos dt == wkDayPos -- 3)
    ]
  where
    date = dtDate $ datetimeToDateTime dt
    findWeekdayPos dt' = toEnum $ day dt' `div` 7


matchEasterHoliday :: Datetime -> EasterHoliday -> Bool
matchEasterHoliday dt (EasterHoliday datetime) =
    dateTuple == easter
  where
    dateTuple = (year dt, month dt, day dt)
    easter = (year datetime, month datetime, day datetime)

-- | Query if a given daate is on a weekday
isWeekday :: Datetime -> Bool
isWeekday = go . getWeekDay . dtDate . datetimeToDateTime
  where
    go = \case
      Saturday -> False
      Sunday   -> False
      _        -> True

-- | Query if a given daate is on a weekend
isWeekend :: Datetime -> Bool
isWeekend = not . isWeekday

-- | Query if a given daate is a business day
isBusiness :: HolidayGen -> Datetime -> Bool
isBusiness hs dt = not (isHoliday hs dt) && not (isWeekend dt)

-------------------------------------------------------------------------------
-- United Kingdom
-------------------------------------------------------------------------------

-- | United Kingdom Bank Holidays
-- <https://www.gov.uk/bank-holidays>
ukHolidays :: Int -> [Holiday]
ukHolidays year =
  [ christmasDay
  , boxingDay
  , newYearsDay
  , goodFriday year
  , easterMonday year
  , earlyMayBank
  , springBank
  , lateSummerBank
  ]

lonTz :: TimezoneOffset
lonTz = TimezoneOffset 0

boxingDay = Fixed $ FixedHoliday 26 December Nearest_workday lonTz

-- Bank Holidays

earlyMayBank   = Rule (HolidayRule May First Monday)
springBank     = Rule (HolidayRule May Last Monday)
lateSummerBank = Rule (HolidayRule August Last Monday)

-------------------------------------------------------------------------------
-- United States ( NYSE )
-------------------------------------------------------------------------------

-- | United States NYSE Stock Exchange Holidays
-- <https://www.nyse.com/markets/hours-calendars>
nyseHolidays :: Int -> [Holiday]
nyseHolidays year =
  [ independenceDay
  , christmasDay
  , memorialDay
  , newYearsDay
  , martinlutherDay
  , laborDay
  , presidentsDay
  , thanksgivingDay
  , goodFriday year
  ]

nycTz :: TimezoneOffset
nycTz = TimezoneOffset 300

independenceDay = Fixed (FixedHoliday 4 July Nearest_workday nycTz)
memorialDay     = Rule (HolidayRule May Last Monday)
martinlutherDay = Rule (HolidayRule January Third Monday)
laborDay        = Rule (HolidayRule September First Monday)
presidentsDay   = Rule (HolidayRule February Third Monday)
thanksgivingDay = Rule (HolidayRule November Fourth Thursday)

-------------------------------------------------------------------------------
-- Easter Holidays
-------------------------------------------------------------------------------

goodFriday, easterMonday :: Int -> Holiday
goodFriday = Easter . goodFriday'
easterMonday = Easter . easterMonday'

goodFriday' :: Int -> EasterHoliday
goodFriday' yr = EasterHoliday $ sub dt (days 2)
  where
    EasterHoliday dt = easterSunday' yr

easterMonday' :: Int -> EasterHoliday
easterMonday' yr = EasterHoliday $ add dt (days 1)
  where
    EasterHoliday dt = easterSunday' yr

easterSunday' :: Int -> EasterHoliday
easterSunday' yr = EasterHoliday datetime
  where
    (_ ,mo,day) = toGregorian $ gregorianEaster $ fromIntegral yr
    month = toEnum $ mo - 1
    dateTime = DateTime (Date yr month day) (TimeOfDay 0 0 0 0)
    datetime = dateTimeToDatetime dateTime

-------------------------------------------------------------------------------
-- Generic Holidays
-------------------------------------------------------------------------------

christmasDay    = Fixed (FixedHoliday 25 December Nearest_workday nycTz)
newYearsDay     = Fixed (FixedHoliday 1 January Next_monday nycTz)
