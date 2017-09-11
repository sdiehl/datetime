<p align="center">
  <img src="https://www.adjoint.io/images/logo-small.png" width="250"/>
</p>

Calendars
=========

[![CircleCI](https://circleci.com/gh/adjoint-io/datetime.svg?style=svg&circle-token=dbb93d30a3189d5d3b3c34ca92d25d226bc00fea)](https://circleci.com/gh/adjoint-io/datetime)

A library for financial datetime manipulations.

Example data structures:

```haskell
import Holiday
import Holiday.Types
import Data.Aeason as A

christmas :: Datetime
christmas = Datetime
  { year     = 2017
  , month    = 12
  , day      = 25
  , hour     = 0
  , minute   = 0
  , second   = 0
  , zone     = 0
  , week_day = 1
  }

q1, q2, q3, a4 :: Interval
q1 = Interval (fomonth 2017 January) (eomonth 2017 March)
q2 = Interval (fomonth 2017 April) (eomonth 2017 June)
q3 = Interval (fomonth 2017 July) (eomonth 2017 September)
q4 = Interval (fomonth 2017 October) (eomonth 2017 December)

eom :: Datetime
eom = eomonth 2017 March

eos :: Datetime
eos = eomonth 2017 September
```

Example date time calculation for calendarization, difference calculation and
holiday and market status:

```haskell
main :: IO ()
main = do
  print (isHoliday christmas)
  print (isWeekend christmas)
  putStrLn (A.encode christmas)

  print (isBusiness eom)
  print (daysBetween (eomonth 2017 March) (eomonth 2019 March))
  print (christmas `within` q4)

  nowDt <- now
  print nowDt

  print $ nowDt `add` (months 3)
  print $ nowDt `add` ((months 3) <> (days 3))
```

The holidays for United Kingdom and United States banking days can be generated
programatically from recurrence rules:

```haskell
> ukHolidays 2017
[  Fixed (FixedHoliday {recurrenceDay = 25, recurrenceMonth = December, observance = Nearest_workday, timezone = +0500})
,  Fixed (FixedHoliday {recurrenceDay = 26, recurrenceMonth = December, observance = Nearest_workday, timezone = +0000})
,  Fixed (FixedHoliday {recurrenceDay = 1, recurrenceMonth = January, observance = Next_monday, timezone = +0500})
,  Easter (EasterHoliday (Datetime {year = 2017, month = 4, day = 14, hour = 0, minute = 0, second = 0, zone = 0, week_day = 5}))
,  Easter (EasterHoliday (Datetime {year = 2017, month = 4, day = 17, hour = 0, minute = 0, second = 0, zone = 0, week_day = 1}))
,  Rule (HolidayRule {monthOfYear = May, weekDayPos = First, weekDay = Monday})
,  Rule (HolidayRule {monthOfYear = May, weekDayPos = Last, weekDay = Monday})
,  Rule (HolidayRule {monthOfYear = August, weekDayPos = Last, weekDay = Monday})
]
```

The datetime type also implements a full serialize and deserializer for JSON
encoding and binary wire protocol serialization.

License
-------

Copyright 2017 Adjoint Inc

Released under Apache 2.0.
