module Service.DateHelpers
  ( addMinutes
  , dayFromHour
  , getCurrentDateString
  , minuteDiff
  , todayFromHour
  , utcTimeToCronInstant
  )
where

import Prelude hiding (min)

import Data.Fixed (Pico)
import qualified Data.Time.Clock as C
import Data.Time.Clock (NominalDiffTime, UTCTime)
import qualified Data.Time.Format as F
import Data.Time.Format (FormatTime)
import qualified Data.Time.Format.ISO8601 as ISO

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
  formatForDateString <$> C.getCurrentTime

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
