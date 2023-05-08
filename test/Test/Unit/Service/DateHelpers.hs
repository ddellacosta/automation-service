{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Test.Unit.Service.DateHelpers
  ( spec
  ,
  )
where

import Control.Lens (filtered, preview)
import Data.Aeson (Value(String))
import Data.Aeson.Lens (_String, key, values)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Time.Clock as C
import Data.Time.Clock (UTCTime)
import qualified Data.Time.Format.ISO8601 as ISO
import Test.Helpers (loadOneDay)
import Test.Hspec (Spec, describe, it, shouldBe)
import Service.DateHelpers
  ( addMinutes
  , dayFromHour
  , getSunriseAndSunset
  , utcTimeToCronInstant
  )
import qualified System.Cron.Parser as P
import System.Environment (setEnv)

spec :: Spec
spec = describe "date utility functions" $ do
  it "generates a valid cron-format schedule from parsed dates" $ do
    oneDay <- loadOneDay

    utcNow <- C.getCurrentTime

    let
      testDate = fromMaybe utcNow $
        ISO.iso8601ParseM "2023-05-01T00:00:00Z"

    sunrise <- fromMaybe utcNow <$> mkUtcTimeFromVal "Rise" testDate oneDay
    sunset <- fromMaybe utcNow <$> mkUtcTimeFromVal "Set" testDate oneDay

    --
    -- Using `show` here and below because producing a `CronSchedule`
    -- explicitly is a pain in the ass
    --
    show (P.parseCronSchedule . T.pack . utcTimeToCronInstant $ sunrise)
      `shouldBe`
      "Right CronSchedule 53 9 1 5 1"

    show (P.parseCronSchedule . T.pack . utcTimeToCronInstant $ sunset)
      `shouldBe`
      "Right CronSchedule 54 23 1 5 1"

    let
      thirtyAfterSunrise = addMinutes 30 sunrise
      thirtyBeforeSunset = addMinutes (-30) sunset

    show (P.parseCronSchedule . T.pack . utcTimeToCronInstant $ thirtyAfterSunrise)
      `shouldBe`
      "Right CronSchedule 23 10 1 5 1"

    show (P.parseCronSchedule . T.pack . utcTimeToCronInstant $ thirtyBeforeSunset)
      `shouldBe`
      "Right CronSchedule 24 23 1 5 1"

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
          ISO.iso8601ParseM "2023-05-08T20:00:00+00:00"
      sunEvents@(sunrise, sunset) <- getSunriseAndSunset testDate coords

      -- sunset should not be before sunrise
      sunset < sunrise `shouldBe` False

      (show sunEvents)
        `shouldBe`
        case tzStr of
          "America/New_York" ->
            "(Just 2023-05-08 09:44:34.938793480396 UTC,Just 2023-05-09 00:01:04.509460330009 UTC)"
          "Europe/Kyiv" ->
            "(Just 2023-05-09 02:20:05.546441674232 UTC,Just 2023-05-09 17:29:46.558659374713 UTC)"
          "Asia/Tokyo" ->
            "(Just 2023-05-09 19:41:06.513182222843 UTC,Just 2023-05-10 09:34:51.927377879619 UTC)"
          "Africa/Cairo" ->
            "(Just 2023-05-09 03:06:03.68356347084 UTC,Just 2023-05-09 16:37:30.942675769329 UTC)"
          "America/Mendoza" ->
            "(Just 2023-05-08 11:13:29.21167731285 UTC,Just 2023-05-08 21:49:50.850725769996 UTC)"
          "Pacific/Honolulu" ->
            "(Just 2023-05-08 15:56:27.287977337837 UTC,Just 2023-05-09 04:59:31.597565710544 UTC)"
          "Pacific/Auckland" ->
            "(Just 2023-05-09 19:07:33.402958810329 UTC,Just 2023-05-10 05:27:09.667979478836 UTC)"

  where
    mkUtcTimeFromVal :: Text -> UTCTime -> Value -> IO (Maybe UTCTime)
    mkUtcTimeFromVal sundataKey date oneDay =
      pure $ flip dayFromHour date $ T.unpack hourString
      where
        hourString = fromMaybe "00:00" $ sundataVal sundataKey oneDay

    --
    -- This is duplicated in Service.Automation.LuaScript. I will pull
    -- it out when it makes sense to do so, but for now this is
    -- preferable to exporting that function so I can use it here, or
    -- creating a new module just for handling the output of the
    -- aa.usno.navy.mil API.
    --
    sundataVal :: Text -> Value -> Maybe Text
    sundataVal sdk = preview
      ( key "properties"
      . key "data"
      . key "sundata"
      . values
      . filtered ((== Just (String sdk)) . preview (key "phen"))
      . key "time"
      . _String
      )
