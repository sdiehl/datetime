module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.List (unfoldr)
import Data.Hourglass
import Data.Hourglass.Types

import Holiday
import Holiday.Types

nyse2017Holidays = map dateTimeToDatetime
  [ DateTime (Date 2017 January 2)   (TimeOfDay 0 0 0 0)
  , DateTime (Date 2017 January 16)  (TimeOfDay 0 0 0 0)
  , DateTime (Date 2017 February 20) (TimeOfDay 0 0 0 0)
  , DateTime (Date 2017 April 14)    (TimeOfDay 0 0 0 0)
  , DateTime (Date 2017 May 29)      (TimeOfDay 0 0 0 0)
  , DateTime (Date 2017 July 4)      (TimeOfDay 0 0 0 0)
  , DateTime (Date 2017 September 4) (TimeOfDay 0 0 0 0)
  , DateTime (Date 2017 November 23) (TimeOfDay 0 0 0 0)
  , DateTime (Date 2017 December 25) (TimeOfDay 0 0 0 0)
  ]

suite :: TestTree
suite = testGroup "Test Suite"
  [ testCase "Correctly identifies Holidays" $ do
      let jan1st2017 = dateTimeToDatetime $
            DateTime (Date 2017 January 1) (TimeOfDay 0 0 0 0)
      let yearDts = take 365 $
            flip unfoldr jan1st2017 $ \dt ->
              Just (dt, add dt (days 1))

      let currYear = year jan1st2017

      -- Assert correct # of days filtered from past year as NYSE Holidays
      let nyseHolidays' = filter isNYSEHoliday yearDts
      assertEqual
        "Correct NYSE holidays found"
        nyse2017Holidays
        nyseHolidays'

      assertEqual
        "Correct number of NYSE holidays found"
        (length $ nyseHolidays currYear)
        (length nyseHolidays')

      -- Assert correct # of days filtered from past year as UK Holidays
      let ukHolidays' = filter isUKHoliday yearDts
      assertEqual
        "Correct number of UK holidays found"
        (length $ ukHolidays currYear)
        (length ukHolidays')
  ]

main :: IO ()
main = defaultMain suite
