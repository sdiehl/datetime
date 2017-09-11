<p align="center">
  <img src="https://www.adjoint.io/images/logo-small.png" width="250"/>
</p>

Datetimes
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
  print (daysBetween (eomonth 2017 March) (eomonth 2019 March ))

  nowDt <- now
  print nowDt

  print $ nowDt `add` (months 3)
  print $ nowDt `add` ((months 3) <> (days 3))
```

License
-------

Copyright 2017 Adjoint Inc

Released under Apache 2.0.
