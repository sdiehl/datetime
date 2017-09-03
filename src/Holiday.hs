module Holiday (
  isUKHoliday,
  isNYSEHoliday,
) where

import Data.Hourglass

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

data Holiday
  = Fixed FixedHoliday
  | Rule
  deriving (Show)

-- XXX: use days() function to do arithmetic
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
-- United Kingdom
-------------------------------------------------------------------------------

isUKHoliday :: [Holiday]
isUKHoliday = []

lonTz :: TimezoneOffset
lonTz = TimezoneOffset 0

boxingDay    = FixedHoliday 26 December Nearest_workday lonTz
goodFriday   = Rule
easterMonday = Rule

-------------------------------------------------------------------------------
-- United States ( NYSE )
-------------------------------------------------------------------------------

isNYSEHoliday :: [Holiday]
isNYSEHoliday = [
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
laborDay        = Rule
presidentsDay   = Rule
thanksgivingDay = Rule
