module Test.Unit.Service.DateHelpers
  ( spec
  ,
  )
where

import Control.Lens (filtered, preview)
import Data.Aeson (Value(String))
import Data.Aeson.Lens (_String, key, values)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Time.Format.ISO8601 as ISO
import qualified Data.Time.LocalTime as LT
import Data.Time.LocalTime (ZonedTime)
import Test.Helpers (loadOneDay)
import Test.Hspec (Spec, describe, it, shouldBe)
import Service.DateHelpers
  ( addMinutes
  , dayFromHour
  , zonedTimeToCronInstant
  )
import qualified System.Cron.Parser as P

spec :: Spec
spec = describe "date utility functions" $ do
  it "generates a valid cron-format schedule from parsed dates" $ do
    oneDay <- loadOneDay

    placeholder <- LT.getZonedTime

    let
      testDate = fromMaybe placeholder $
        ISO.iso8601ParseM "2023-05-01T00:00:00-04:00"

    sunrise <- fromMaybe placeholder <$> mkZonedTimeFromVal "Rise" testDate oneDay
    sunset <- fromMaybe placeholder <$> mkZonedTimeFromVal "Set" testDate oneDay

    --
    -- Using `show` here and below because producing a `CronSchedule`
    -- explicitly is a pain in the ass
    --
    show (P.parseCronSchedule . T.pack . zonedTimeToCronInstant $ sunrise)
      `shouldBe`
      "Right CronSchedule 53 5 1 5 1"

    show (P.parseCronSchedule . T.pack . zonedTimeToCronInstant $ sunset)
      `shouldBe`
      "Right CronSchedule 54 19 1 5 1"

    thirtyAfterSunrise <- addMinutes 30 sunrise
    thirtyBeforeSunset <- addMinutes (-30) sunset

    show (P.parseCronSchedule . T.pack . zonedTimeToCronInstant $ thirtyAfterSunrise)
      `shouldBe`
      "Right CronSchedule 23 6 1 5 1"

    show (P.parseCronSchedule . T.pack . zonedTimeToCronInstant $ thirtyBeforeSunset)
      `shouldBe`
      "Right CronSchedule 24 19 1 5 1"

  where
    mkZonedTimeFromVal :: Text -> ZonedTime -> Value -> IO (Maybe ZonedTime)
    mkZonedTimeFromVal sundataKey date oneDay = do
      flip dayFromHour date $ T.unpack hourString
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
