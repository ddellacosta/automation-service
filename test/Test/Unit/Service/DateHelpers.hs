module Test.Unit.Service.DateHelpers
  ( spec
  ,
  )
where

import Control.Lens (filtered, preview)
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens (_String, key, values)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Time.Clock as C
import Test.Helpers (loadOneDay)
import Test.Hspec (Spec, describe, it, shouldBe)
import Service.DateHelpers
  ( addMinutes
  , getFormattedLocalDate
  , localTimeToCronInstant
  , parseUTCTime
  , toLocalTime
  )
import qualified System.Cron.Parser as P

spec :: Spec
spec = describe "date utility functions" $ do
  it "generates a valid cron-format schedule from parsed dates" $ do
    oneDay <- loadOneDay

    currentDate <- T.pack <$> getFormattedLocalDate

    let
      sunrise = mkTodayUTCTime "Rise" currentDate oneDay
      sunset = mkTodayUTCTime "Set" currentDate oneDay

    sunrise' <- traverse toLocalTime sunrise
    sunset' <- traverse toLocalTime sunset

    --
    -- Using `show` here and below because producing a `CronSchedule`
    -- explicitly is a pain in the ass
    --
    show (P.parseCronSchedule . T.pack . localTimeToCronInstant <$> sunrise')
      `shouldBe`
      "Just (Right CronSchedule 53 5 1 5 1)"

    show (P.parseCronSchedule . T.pack . localTimeToCronInstant <$> sunset')
      `shouldBe`
      "Just (Right CronSchedule 54 19 1 5 1)"

    thirtyAfterSunrise <- traverse toLocalTime $ addMinutes 30 <$> sunrise
    thirtyBeforeSunset <- traverse toLocalTime $ addMinutes (-30) <$> sunset

    show (P.parseCronSchedule . T.pack . localTimeToCronInstant <$> thirtyAfterSunrise)
      `shouldBe`
      "Just (Right CronSchedule 23 6 1 5 1)"

    show (P.parseCronSchedule . T.pack . localTimeToCronInstant <$> thirtyBeforeSunset)
      `shouldBe`
      "Just (Right CronSchedule 24 19 1 5 1)"

  where
    sundataVal :: T.Text -> Aeson.Value -> Maybe T.Text
    sundataVal sdk = preview
      ( key "properties"
      . key "data"
      . key "sundata"
      . values
      . filtered ((== Just (Aeson.String sdk)) . preview (key "phen"))
      . key "time"
      . _String
      )

    mkTodayUTCTime :: T.Text -> T.Text -> Aeson.Value -> Maybe C.UTCTime
    mkTodayUTCTime sundataKey dateString oneDay = parseUTCTime . T.unpack $
      dateString <> "T" <> (fromMaybe "00:00" $ sundataVal sundataKey oneDay) <> ":00Z"
