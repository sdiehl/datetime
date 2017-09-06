{-# LANGUAGE LambdaCase #-}

module Holiday (

  isWeekend,
  isWeekday,
  isBusiness,

  ukHolidays,
  nyseHolidays,

) where

import Data.Either (partitionEithers)
import Data.Hourglass

import Holiday.Types

data HolidaySet
  = UnitedKingdom [Holiday]
  | NYSE [Holiday]
  deriving (Show)

data Observance
  = Nearest_workday         -- move Saturday to Friday and Sunday to Monday
  | Sunday_to_monday        -- move Sunday to following Monday
  | Next_monday_or_tuesday  -- move Saturday to Monday and Sunday/Monday to Tuesday
  | Previous_friday         -- move Saturday and Sunday to previous Friday
  | Next_monday             -- move Saturday and Sunday to following Monday
  | None                    -- always on a fixed date
  deriving (Show)

data FixedHoliday = FixedHoliday
  { recurrenceDay   :: Int
  , recurrenceMonth :: Month
  , observance      :: Observance
  , timezone        :: TimezoneOffset
  } deriving (Show)

-- | Specifies a recurrence rule for holidays that have a logical yearly
-- recurrence date rather than a fixed date every year.
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

data Holiday
  = Fixed FixedHoliday
  | Rule HolidayRule
  deriving (Show)

partitionHolidays :: [Holiday] -> ([FixedHoliday],[HolidayRule])
partitionHolidays = partitionEithers . map partition'
  where
    partition' (Fixed fixed) = Left fixed
    partition' (Rule rule) = Right rule

-- XXX: use days() function to do arithmetic & replace with Datetime
observedShift :: Observance -> Date -> Date
observedShift obs dt = case obs of

  Nearest_workday -> case getWeekDay dt of
    Saturday -> dt { dateDay = day - 1 } -- prev friday
    Sunday   -> dt { dateDay = day + 1 } -- next monday
    _        -> dt

  Sunday_to_monday -> case getWeekDay dt of
    Sunday   -> dt { dateDay = day + 1 } -- next monady
    _        -> dt

  Next_monday_or_tuesday -> case getWeekDay dt of
    Saturday   -> dt { dateDay = day + 2 } -- next monday
    Sunday     -> dt { dateDay = day + 2 } -- next tuesday
    Monday     -> dt { dateDay = day + 1 } -- next tuesday
    _        -> dt

  Previous_friday -> case getWeekDay dt of
    Saturday   -> dt { dateDay = day - 1 } -- previous friday
    Sunday     -> dt { dateDay = day - 2 } -- previous friday
    _        -> dt

  Next_monday -> case getWeekDay dt of
    Saturday   -> dt { dateDay = day + 2 } -- next monday
    Sunday     -> dt { dateDay = day + 1 } -- next monday
    _        -> dt

  None            -> dt

  where
    day = dateDay dt

-------------------------------------------------------------------------------
-- Queries
-------------------------------------------------------------------------------

isHoliday :: Datetime -> Bool
isHoliday dt = isUKHoliday dt || isNYSEHoliday dt

-- | XXX Add FixedHoliday (|| any $ matchFixedHoliday ..)
isUKHoliday :: Datetime -> Bool
isUKHoliday dt =
    any (matchHolidayRule dt) ruleHolidays
  where
    (UnitedKingdom holidays) = ukHolidays
    (fixedHolidays, ruleHolidays) = partitionHolidays holidays

-- | XXX Add FixedHoliday (|| any $ matchFixedHoliday ..)
isNYSEHoliday :: Datetime -> Bool
isNYSEHoliday dt =
    any (matchHolidayRule dt) ruleHolidays
  where
    (NYSE holidays) = nyseHolidays
    (fixedHolidays, ruleHolidays) = partitionHolidays holidays

-- | A Datetime matches the Holiday rule iff:
-- 1) the weekday of the datetime is the same as in the holiday rule
-- 2) the month of the datetime is the same as in the holiday rule
-- 3) the day `wkNum - 1` weeks ago is within the same month as the holiday rule
matchHolidayRule :: Datetime -> HolidayRule -> Bool
matchHolidayRule dt' (HolidayRule month wkNum wkDay)
  | getWeekDay dt /= wkDay = False
  | dateMonth dt /= month  = False
  | otherwise =
      let dT = datetimeToDateTime (sub dt' $ weeks (wkNum - 1))
      in dateMonth (dtDate dT) == month
  where
    dt = dtDate $ datetimeToDateTime dt'

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

ukHolidays :: HolidaySet
ukHolidays = UnitedKingdom [boxingDay]

lonTz :: TimezoneOffset
lonTz = TimezoneOffset 0

boxingDay    = Fixed $ FixedHoliday 26 December Nearest_workday lonTz
-- goodFriday   = Rule (
-- easterMonday = Rule

-------------------------------------------------------------------------------
-- United States ( NYSE )
-------------------------------------------------------------------------------

nyseHolidays :: HolidaySet
nyseHolidays = NYSE [
    christmasDay
  , independenceDay
  , memorialDay
  , newYearsDay
  , laborDay
  , presidentsDay
  , thanksgivingDay
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
