{-# LANGUAGE LambdaCase #-}

module Holiday (
  HolidaySet(..),
  HolidayRule(..),
  Observance(..),

  module Time.Types,

  isWeekend,
  isWeekday,
  isBusiness,

  ukHolidays,
  nyseHolidays,
) where

import Protolude

import Data.Hourglass
import Data.Time.Calendar (toGregorian)
import Data.Time.Calendar.Easter (gregorianEaster)
import qualified Time.Types

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
  deriving (Show)

data FixedHoliday = FixedHoliday
  { recurrenceDay   :: Int
  , recurrenceMonth :: Month
  , observance      :: Observance
  , timezone        :: TimezoneOffset
  } deriving (Show)

-- | Specifies a recurrence rule for holidays that have a logical yearly
-- recurrence date rather than a fixed date every year. YEAR recurrence
-- is implicit, therefore no recurrence rule is specified.
data HolidayRule = HolidayRule
  { monthOfYear :: Month
  , weekOfMonth :: Int
  , dayOfWeek   :: WeekDay
  } deriving (Show)

-- | Smart constructor for `HolidayRule`, trims weeks to max 5, min 0
mkHolidayRule :: Month -> Int -> WeekDay -> HolidayRule
mkHolidayRule month week' day = HolidayRule month week day
  where
    week = min 5 $ max 0 week'

data EasterHoliday = EasterHoliday Datetime
  deriving (Show)

data Holiday
  = Fixed FixedHoliday
  | Rule HolidayRule
  | Easter EasterHoliday
  deriving (Show)

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

isHoliday :: Datetime -> Bool
isHoliday dt = isUKHoliday dt || isNYSEHoliday dt

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
  [ fromEnum fmonth == month shiftedDt
  , fday          == day shiftedDt
  -- XXX , match timezone here
  ]
  where
    shiftedDt = observedShift obs dt

-- | A Datetime matches the Holiday rule iff:
-- 1) the weekday of the datetime is the same as in the holiday rule
-- 2) the month of the datetime is the same as in the holiday rule
-- 3) the day `wkNum - 1` weeks ago is within the same month as the holiday rule
matchHolidayRule :: Datetime -> HolidayRule -> Bool
matchHolidayRule dt (HolidayRule month wkNum wkDay) = and
    [ getWeekDay date       == wkDay -- 1)
    , dateMonth date        == month -- 2)
    , dateMonth (dtDate dT) == month -- 3)
    ]
  where
    date = dtDate $ datetimeToDateTime dt
    dT = datetimeToDateTime (sub dt $ weeks (wkNum - 1))

matchEasterHoliday :: Datetime -> EasterHoliday -> Bool
matchEasterHoliday dt (EasterHoliday datetime) = dt == datetime

isWeekday :: Datetime -> Bool
isWeekday = go . getWeekDay . dtDate . datetimeToDateTime
  where
    go = \case
      Saturday -> False
      Sunday   -> False
      _        -> True

isWeekend :: Datetime -> Bool
isWeekend = not . isWeekday

isBusiness :: Datetime -> Bool
isBusiness dt = not (isHoliday dt) && not (isWeekend dt)

-------------------------------------------------------------------------------
-- United Kingdom
-------------------------------------------------------------------------------

-- | United Kingdom Bank Holidays
-- <https://www.gov.uk/bank-holidays>
ukHolidays :: Int -> [Holiday]
ukHolidays year =
  [ boxingDay
  , goodFriday year
  , easterMonday year
  ]

lonTz :: TimezoneOffset
lonTz = TimezoneOffset 0

boxingDay = Fixed $ FixedHoliday 26 December Nearest_workday lonTz

-------------------------------------------------------------------------------
-- United States ( NYSE )
-------------------------------------------------------------------------------

-- | United States NYSE Stock Exchange Holidays
-- <https://www.nyse.com/markets/hours-calendars>
nyseHolidays :: Int -> [Holiday]
nyseHolidays year =
  [ christmasDay
  , independenceDay
  , memorialDay
  , newYearsDay
  , laborDay
  , presidentsDay
  , thanksgivingDay
  , goodFriday year
  ]

nycTz :: TimezoneOffset
nycTz = TimezoneOffset 300

christmasDay    = Fixed (FixedHoliday 25 December Nearest_workday nycTz)
independenceDay = Fixed (FixedHoliday 4 July Nearest_workday nycTz)
memorialDay     = Fixed (FixedHoliday 25 May None nycTz)
newYearsDay     = Fixed (FixedHoliday 1 January None nycTz)
laborDay        = Rule (mkHolidayRule September 1 Monday) -- first monday in september
presidentsDay   = Rule (mkHolidayRule February 3 Monday)  -- third monday in February
thanksgivingDay = Rule (mkHolidayRule November 4 Monday)  -- fourth thursday in November

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
