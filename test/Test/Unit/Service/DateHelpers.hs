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
import qualified Data.Time.Clock as C
import Data.Time.Clock (UTCTime)
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
import System.Environment (setEnv)

spec :: Spec
spec = describe "date utility functions" $ do
  it "generates a valid cron-format schedule from parsed dates" $ do
    oneDay <- loadOneDay

    -- I've had to add this to this test so it passes when I'm running
    -- nix build, because I can't seem to set the timezone there, but
    -- if I set this to UTC these tests will pass
    -- locally. Probably something stupid, as always...
    -- Anyways, unfortunately, this fails to show how it automatically
    -- converts to the local timezone, but I suppose that's implied by
    -- the types and functions used.
    setEnv "TZ" "UTC"

    zonedNow <- LT.getZonedTime
    utcNow <- C.getCurrentTime

    let
      testDate = fromMaybe utcNow $
        ISO.iso8601ParseM "2023-05-01T00:00:00Z"

    sunrise <- fromMaybe zonedNow <$> mkZonedTimeFromVal "Rise" testDate oneDay
    sunset <- fromMaybe zonedNow <$> mkZonedTimeFromVal "Set" testDate oneDay

    --
    -- Using `show` here and below because producing a `CronSchedule`
    -- explicitly is a pain in the ass
    --
    show (P.parseCronSchedule . T.pack . zonedTimeToCronInstant $ sunrise)
      `shouldBe`
      "Right CronSchedule 53 9 1 5 1"

    show (P.parseCronSchedule . T.pack . zonedTimeToCronInstant $ sunset)
      `shouldBe`
      "Right CronSchedule 54 23 1 5 1"

    thirtyAfterSunrise <- addMinutes 30 sunrise
    thirtyBeforeSunset <- addMinutes (-30) sunset

    show (P.parseCronSchedule . T.pack . zonedTimeToCronInstant $ thirtyAfterSunrise)
      `shouldBe`
      "Right CronSchedule 23 10 1 5 1"

    show (P.parseCronSchedule . T.pack . zonedTimeToCronInstant $ thirtyBeforeSunset)
      `shouldBe`
      "Right CronSchedule 24 23 1 5 1"

  where
    mkZonedTimeFromVal :: Text -> UTCTime -> Value -> IO (Maybe ZonedTime)
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
