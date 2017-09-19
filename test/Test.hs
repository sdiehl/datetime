{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.QuickCheck

import Test.QuickCheck.Monadic

import Data.List (unfoldr)
import Data.Hourglass
import Data.Hourglass.Types
import Data.Monoid ((<>))
import Datetime
import Datetime.Types


instance Arbitrary Datetime where
  arbitrary = posixToDatetime <$> choose (1, 32503680000) -- (01/01/1970, 01/01/3000)

instance Arbitrary TimezoneOffset where
  arbitrary = TimezoneOffset <$> choose (-660, 840)

nyse2017Holidays = map (dateTimeToDatetime timezone_UTC)
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

uk2017Holidays = map (dateTimeToDatetime timezone_UTC)
  [ DateTime (Date 2017 January 2)   (TimeOfDay 0 0 0 0)
  , DateTime (Date 2017 April 14)    (TimeOfDay 0 0 0 0)
  , DateTime (Date 2017 April 17)    (TimeOfDay 0 0 0 0)
  , DateTime (Date 2017 May 1)       (TimeOfDay 0 0 0 0)
  , DateTime (Date 2017 May 29)      (TimeOfDay 0 0 0 0)
  , DateTime (Date 2017 August 28)   (TimeOfDay 0 0 0 0)
  , DateTime (Date 2017 December 25) (TimeOfDay 0 0 0 0)
  , DateTime (Date 2017 December 26) (TimeOfDay 0 0 0 0)
  ]

suite :: TestTree
suite = testGroup "Test Suite"
  [ testCase "Correctly identifies Holidays" $ do
      let jan1st2017 = dateTimeToDatetime timezone_UTC $
            DateTime (Date 2017 January 1) (TimeOfDay 0 0 0 0)
      let currYear = year jan1st2017
      let yearDts = take 365 $
            flip unfoldr jan1st2017 $ \dt ->
              Just (dt, add dt (days 1))

      let nyseHolidays' = filter isNYSEHoliday yearDts

      assertEqual
        "Correct NYSE holidays found"
        nyse2017Holidays
        nyseHolidays'

      assertEqual
        "Correct number of NYSE holidays found"
        (length $ nyseHolidays currYear)
        (length nyseHolidays')
      let ukHolidays' = filter isUKHoliday yearDts

      assertEqual
        "Correct UK holidays found"
        uk2017Holidays
        ukHolidays'

      assertEqual
        "Correct number of UK holidays found"
        (length $ ukHolidays currYear)
        (length ukHolidays')

  , testProperty "Manipulating Timezone preserves Datetime Integrity" $ \tzo ->
      monadicIO $ do
        dtNow <- run now
        let dtNowLocal = alterTimezone tzo dtNow
        let tzoMins = timezoneOffsetToMinutes tzo
        let (hrs',mins') = divMod (abs tzoMins) 60
        let alter = if tzoMins < 0 then sub else add
        let dtNowLocal' = (dtNow `alter` (hours hrs' <> mins mins')) { zone = tzoMins }
        assert $ dtNowLocal == dtNowLocal'

  , testProperty "ISO 8601: parse . format = id" $ \(dt :: Datetime) ->
      (parseDatetime $ formatDatetime dt) == (Just dt)

  , testProperty "dateTimeToDatetime . datetimeToDateTime = id" $ \(dt :: Datetime) ->
      (dateTimeToDatetime timezone_UTC $ datetimeToDateTime dt) == dt

  ]

main :: IO ()
main = defaultMain suite
