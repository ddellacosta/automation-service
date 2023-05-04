module Service.DateHelpers
  ( addMinutes
  , dayFromHour
  , getCurrentDateString
  , getCurrentZonedTime
  , loadCurrentTimeZoneFromEnv
  , minuteDiff
  , todayFromHour
  , zonedTimeToCronInstant
  )
where

import Prelude hiding (min)

import Data.Fixed (Pico)
import qualified Data.Time.Clock as C
import Data.Time.Clock (NominalDiffTime, UTCTime)
import qualified Data.Time.Format as F
import Data.Time.Format (FormatTime)
import qualified Data.Time.Format.ISO8601 as ISO
import qualified Data.Time.LocalTime as LT
import Data.Time.LocalTime (TimeZone, ZonedTime)
import qualified Data.Time.Zones as TZ
import qualified Data.Time.Zones.Files as TZF
import System.Environment (getEnv)

minuteDiff :: Pico -> NominalDiffTime
minuteDiff = C.secondsToNominalDiffTime . (* 60)

getCurrentZonedTime :: IO ZonedTime
getCurrentZonedTime = do
  currentUTC <- C.getCurrentTime
  tz <- loadCurrentTimeZoneFromEnv
  pure $ LT.utcToZonedTime tz currentUTC

zonedTimeToCronInstant :: ZonedTime -> String
zonedTimeToCronInstant =
  F.formatTime F.defaultTimeLocale "%M %H %d %m %w"

addMinutes :: Pico -> ZonedTime -> IO ZonedTime
addMinutes min initial = LT.utcToLocalZonedTime updatedUTC
  where
    initialUTC = LT.zonedTimeToUTC initial
    updatedUTC = C.addUTCTime (minuteDiff min) initialUTC

-- |
-- | Returns a (Maybe-wrapped) ZonedTime value for the given hour with
-- today's date, using the timezone returned by getCurrentTimeZone.
--
todayFromHour :: String -> IO (Maybe ZonedTime)
todayFromHour hourString = dayFromHour hourString =<< C.getCurrentTime

-- | Returns the current date in the format "2023-05-01"
getCurrentDateString :: IO String
getCurrentDateString = do
  utc <- C.getCurrentTime
  tz <- loadCurrentTimeZoneFromEnv
  pure $ formatForDateString $ LT.utcToZonedTime tz utc

formatForDateString :: (FormatTime ts) => ts -> String
formatForDateString = F.formatTime F.defaultTimeLocale "%Y-%m-%d"

dayFromHour :: String -> UTCTime -> IO (Maybe ZonedTime)
dayFromHour hourString date = do
  let dateString = formatForDateString date
  tz <- loadCurrentTimeZoneFromEnv
  let
    utcWithHours =
      ISO.iso8601ParseM (dateString <> "T" <> hourString <> ":00Z") :: Maybe UTCTime
  pure $ LT.utcToZonedTime tz <$> utcWithHours

loadCurrentTimeZoneFromEnv :: IO TimeZone
loadCurrentTimeZoneFromEnv = do
  tzVar <- getEnv "TZ"
  tzPath <- TZF.timeZonePathFromDB tzVar
  tz <- TZ.loadTZFromFile tzPath
  utcNow <- C.getCurrentTime
  pure $ TZ.timeZoneForUTCTime tz utcNow
