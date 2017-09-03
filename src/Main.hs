module Main where

data Datetime = Datetime
  { year     :: Int -- the complete year
  , month    :: Int -- a month, between 1 and 12
  , day      :: Int -- a day, between 1 and 31
  , hour     :: Int -- the number of hours since midnight, between 0 and 23
  , minute   :: Int -- the number of minutes since the beginning of the hour, between 0 and 59
  , second   :: Int -- the number of seconds since the begining of the minute, between 0 and 59
  , zone     :: Int -- the local zone offset, in minutes of advance wrt UTC.
  , week_day :: Int -- the number of days since sunday, between 0 and 6 Note As a special exception, week_day may be -1, if the day of the week is unknown	*)
  }

main = return ()
