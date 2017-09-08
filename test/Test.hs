module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.List (unfoldr)
import Data.Hourglass
import Data.Hourglass.Types

import Holiday
import Holiday.Types


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
