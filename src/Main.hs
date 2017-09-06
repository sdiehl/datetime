module Main where

import Holiday.Types
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

composite :: Delta
composite = days 3 <> months 4 <> years 1

example :: IO Date
example = do
  dt <- nowDate
  pure (dateAddPeriod (dtDate dt) $ dPeriod composite)

main :: IO ()
main = return ()
