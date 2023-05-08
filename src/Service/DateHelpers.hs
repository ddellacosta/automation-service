{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Service.DateHelpers
  ( addMinutes
  , dayFromHour
  , getCurrentDateString
  , getCurrentZonedTime
  , getSunriseAndSunset
  , getTimeZone
  , minuteDiff
  , todayFromHour
  , utcTimeToCronInstant
  )
where

import Prelude hiding (min)

import Control.Exception (IOException, handle)
import Control.Lens ((^?), _1, _Just)
import qualified Data.Astro.Sun as AstroSun
import qualified Data.Astro.Time.Conv as AstroConv
import qualified Data.Astro.Types as AstroTypes
import Data.Fixed (Pico)
import qualified Data.Time.Clock as C
import Data.Time.Clock (NominalDiffTime, UTCTime)
import qualified Data.Time.Format as F
import Data.Time.Format (FormatTime)
import qualified Data.Time.Format.ISO8601 as ISO
import qualified Data.Time.LocalTime as LT
import Data.Time.LocalTime (TimeZone, ZonedTime)
import qualified Data.Time.Zones as Z
import System.Environment (getEnv)

-- | Given Pico seconds returns a NominalDiffTime for that number of
-- seconds.
--
minuteDiff :: Pico -> NominalDiffTime
minuteDiff = C.secondsToNominalDiffTime . (* 60)

-- | Takes a UTCTime value, presumably in the future, and converts it
-- to a Cron schedule for that specific timestamp. E.g.,
-- "2023-05-05T08:30:00Z" will be converted to "30 08 05 05 5"
--
utcTimeToCronInstant :: UTCTime -> String
utcTimeToCronInstant =
  F.formatTime F.defaultTimeLocale "%M %H %d %m %w"

-- | Given Pico minutes and a UTCTime, returns an updated UTCTime with
-- the minutes added.
--
addMinutes :: Pico -> UTCTime -> UTCTime
addMinutes = C.addUTCTime . minuteDiff

-- | Returns a (Maybe-wrapped) UTCTime value for the given hour with
-- today's date
--
todayFromHour :: String -> IO (Maybe UTCTime)
todayFromHour hourString =
  dayFromHour hourString <$> C.getCurrentTime

-- | Returns the current date in the format "2023-05-01"
--
getCurrentDateString :: IO String
getCurrentDateString =
  formatForDateString <$> getCurrentZonedTime

-- | Returns any timestamp with a FormatTime instance in the format
-- "2023-05-05"
--
formatForDateString :: (FormatTime ts) => ts -> String
formatForDateString = F.formatTime F.defaultTimeLocale "%Y-%m-%d"

-- | Given an hourString in the format "00:00" and a UTCTime, sets the
-- UTCTime hour/minute value to the hourString and returns an updated
-- (Maybe) UTCTime.
--
dayFromHour :: String -> UTCTime -> Maybe UTCTime
dayFromHour hourString date =
  ISO.iso8601ParseM (dateString <> "T" <> hourString <> ":00Z")
  where
    dateString = formatForDateString date

getCurrentZonedTime :: IO ZonedTime
getCurrentZonedTime = C.getCurrentTime >>= getZonedTimeForUTC

getZonedTimeForUTC :: UTCTime -> IO ZonedTime
getZonedTimeForUTC ts = do
  tz <- getTimeZone ts
  pure $ LT.utcToZonedTime tz ts

getTimeZone :: UTCTime -> IO TimeZone
getTimeZone ts = do
  tzVar <- handle (\(_e :: IOException) -> pure "UTC") $ getEnv "TZ"
  tz <- Z.loadTZFromDB tzVar
  pure $ Z.timeZoneForUTCTime tz ts

getSunriseAndSunset :: UTCTime -> (Double, Double) -> IO (Maybe UTCTime, Maybe UTCTime)
getSunriseAndSunset ts (lat, long) = do
  utcTs <- getZonedTimeForUTC ts
  let
    sunEvents@(sunrise, sunset) = getSunriseAndSunset' utcTs (lat, long)

  if sunset < sunrise
    then do
      sunset' <- getNextDaySunset ts (lat, long)
      pure (sunrise, sunset')
    else
      pure sunEvents

  where
    getNextDaySunset :: UTCTime -> (Double, Double) -> IO (Maybe UTCTime)
    getNextDaySunset ts' (lat', long') = do
      nextDay <- getZonedTimeForUTC . addMinutes 1440 $ ts'
      let
        (_sunrise, sunset) = getSunriseAndSunset' nextDay (lat', long')
      pure sunset

getSunriseAndSunset' :: ZonedTime -> (Double, Double) -> (Maybe UTCTime, Maybe UTCTime)
getSunriseAndSunset' ts (lat, long) = (sunrise', sunset')
  where
    coords =
      AstroTypes.GeoC (AstroTypes.DD lat) (AstroTypes.DD long)
    -- see docs for sunRiseAndSet:
    verticalShift = (AstroTypes.DD 0.833333)
    lcd = AstroConv.zonedTimeToLCD ts
    -- fno-warn-incomplete-uni-patterns at the top is for this
    (AstroSun.RiseSet sunrise sunset) = AstroSun.sunRiseAndSet coords verticalShift lcd
    sunrise' = LT.zonedTimeToUTC . AstroConv.lctToZonedTime <$> sunrise ^? _Just . _1
    sunset' = LT.zonedTimeToUTC . AstroConv.lctToZonedTime <$> sunset ^? _Just . _1
