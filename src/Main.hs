module Main where

import Types
--import Holiday

import Data.Monoid
import Data.Hourglass
import Time.System

-------------------------------------------------------------------------------
-- Testing
-------------------------------------------------------------------------------

nowTz :: IO TimezoneOffset
nowTz = timezoneCurrent

nowDate :: IO DateTime
nowDate = dateCurrent

composite :: Period
composite = days 3 <> months 4 <> years 1

example :: IO Date
example = do
  dt <- nowDate
  pure (dateAddPeriod (dtDate dt) composite)

main :: IO ()
main = return ()
