{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Test.Unit.Service.TimeHelpers
  ( spec
  ,
  )
where

import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Time.Clock as C
import qualified Data.Time.Format.ISO8601 as ISO
import Service.TimeHelpers (addMinutes, getSunriseAndSunset, utcTimeToCronInstant)
import qualified System.Cron.Parser as P
import System.Environment (setEnv)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "date utility functions" $ do
  it "generates a valid cron-format schedule from parsed dates" $ do

    setEnv "TZ" "America/New_York"

    utcNow <- C.getCurrentTime

    let
      coords = (41.5020948, (-73.982543))
      testDate = fromMaybe utcNow $
        ISO.iso8601ParseM "2023-05-02T00:00:00Z" -- this will give me 05-01 in EDT

    (Just sunrise, Just sunset) <- getSunriseAndSunset testDate coords

    --
    -- Using `show` here and below because producing a `CronSchedule`
    -- explicitly is a pain in the ass
    --
    show (P.parseCronSchedule . T.pack . utcTimeToCronInstant $ sunrise)
      `shouldBe`
      "Right CronSchedule 53 9 1 5 1"

    show (P.parseCronSchedule . T.pack . utcTimeToCronInstant $ sunset)
      `shouldBe`
      "Right CronSchedule 53 23 1 5 1"

    let
      thirtyAfterSunrise = addMinutes 30 sunrise
      thirtyBeforeSunset = addMinutes (-30) sunset

    show (P.parseCronSchedule . T.pack . utcTimeToCronInstant $ thirtyAfterSunrise)
      `shouldBe`
      "Right CronSchedule 23 10 1 5 1"

    show (P.parseCronSchedule . T.pack . utcTimeToCronInstant $ thirtyBeforeSunset)
      `shouldBe`
      "Right CronSchedule 23 23 1 5 1"

  it "correctly generates sunrise and sunset values according to TZ env var" $ do
    let
      locations =
        [ ("America/New_York", (41.5020948, (-73.982543)))
        , ("Europe/Kyiv", (50.45, 30.523333))
        , ("Asia/Tokyo", (35.689722, 139.692222))
        , ("Africa/Cairo", (30.044444, 31.235833))
        , ("America/Mendoza", ((-32.883333), (-68.816667)))
        , ("Pacific/Honolulu", (21.308889, (-157.826111)))
        , ("Pacific/Auckland", ((-36.840556), 174.74))
        ]

    for_ locations $ \(tzStr, coords) -> do
      setEnv "TZ" tzStr
      utcNow <- C.getCurrentTime

      let
        testDate = fromMaybe utcNow $
          ISO.iso8601ParseM "2023-05-08T20:00:00Z"

      sunEvents@(sunrise, sunset) <- getSunriseAndSunset testDate coords

      -- sunset should not be before sunrise
      sunset < sunrise `shouldBe` False

      (show sunEvents)
        `shouldBe`
        case tzStr of
          "America/New_York" ->
            "(Just 2023-05-08 09:44:34.938793480396 UTC,Just 2023-05-09 00:01:04.509460330009 UTC)"
          "Europe/Kyiv" ->
            "(Just 2023-05-08 02:21:43.064078092575 UTC,Just 2023-05-08 17:28:15.654907822608 UTC)"
          "Asia/Tokyo" ->
            "(Just 2023-05-09 19:41:06.513182222843 UTC,Just 2023-05-10 09:34:51.927377879619 UTC)"
          "Africa/Cairo" ->
            "(Just 2023-05-08 03:06:49.180118143558 UTC,Just 2023-05-08 16:36:51.859363317489 UTC)"
          "America/Mendoza" ->
            "(Just 2023-05-08 11:13:29.21167731285 UTC,Just 2023-05-08 21:49:50.850725769996 UTC)"
          "Pacific/Honolulu" ->
            "(Just 2023-05-08 15:56:27.287977337837 UTC,Just 2023-05-09 04:59:31.597565710544 UTC)"
          "Pacific/Auckland" ->
            "(Just 2023-05-09 19:07:33.402958810329 UTC,Just 2023-05-10 05:27:09.667979478836 UTC)"
