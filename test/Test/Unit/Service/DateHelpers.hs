module Test.Unit.Service.DateHelpers
  ( spec
  ,
  )
where

import qualified Data.Text as T
import Test.Helpers (loadOneDay)
import Test.Hspec (Spec, describe, it, shouldBe)
import Service.Automations.LuaScript (mkUTCTimeFromVal)
import Service.DateHelpers
  ( addMinutes
  , getFormattedLocalDate
  , localTimeToCronInstant
  , toLocalTime
  )
import qualified System.Cron.Parser as P

spec :: Spec
spec = describe "date utility functions" $ do
  it "generates a valid cron-format schedule from parsed dates" $ do
    oneDay <- loadOneDay

    currentDate <- T.pack <$> getFormattedLocalDate

    let
      sunrise = mkUTCTimeFromVal "Rise" currentDate oneDay
      sunset = mkUTCTimeFromVal "Set" currentDate oneDay

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
