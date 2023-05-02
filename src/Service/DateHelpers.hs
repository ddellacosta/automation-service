module Service.DateHelpers
  ( addMinutes
  , dayFromHour
  , getCurrentDateString
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
import qualified Data.Time.Format.ISO8601 as ISO
import qualified Data.Time.LocalTime as LT
import Data.Time.LocalTime (ZonedTime)

minuteDiff :: Pico -> NominalDiffTime
minuteDiff = C.secondsToNominalDiffTime . (* 60)

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
todayFromHour hourString = dayFromHour hourString =<< LT.getZonedTime

-- | Returns the current date in the format "2023-05-01"
getCurrentDateString :: IO String
getCurrentDateString = do
  utc <- C.getCurrentTime
  tz <- LT.getCurrentTimeZone
  pure $ formatForDateString $ LT.utcToZonedTime tz utc

formatForDateString :: ZonedTime -> String
formatForDateString = F.formatTime F.defaultTimeLocale "%Y-%m-%d"

dayFromHour :: String -> ZonedTime -> IO (Maybe ZonedTime)
dayFromHour hourString date = do
  let dateString = formatForDateString date
  tz <- LT.getCurrentTimeZone
  let
    utcWithHours =
      ISO.iso8601ParseM (dateString <> "T" <> hourString <> ":00Z") :: Maybe UTCTime
  pure $ LT.utcToZonedTime tz <$> utcWithHours
