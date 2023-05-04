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
import qualified Data.Time.LocalTime as LT
import System.Environment (getEnv)

minuteDiff :: Pico -> NominalDiffTime
minuteDiff = C.secondsToNominalDiffTime . (* 60)

utcTimeToCronInstant :: UTCTime -> String
utcTimeToCronInstant =
  F.formatTime F.defaultTimeLocale "%M %H %d %m %w"

addMinutes :: Pico -> UTCTime -> UTCTime
addMinutes = C.addUTCTime . minuteDiff

-- |
-- | Returns a (Maybe-wrapped) UTCTime value for the given hour with
-- today's date
--
todayFromHour :: String -> IO (Maybe UTCTime)
todayFromHour hourString =
  C.getCurrentTime >>= pure . dayFromHour hourString

-- | Returns the current date in the format "2023-05-01"
getCurrentDateString :: IO String
getCurrentDateString =
  C.getCurrentTime >>= pure . formatForDateString

formatForDateString :: (FormatTime ts) => ts -> String
formatForDateString = F.formatTime F.defaultTimeLocale "%Y-%m-%d"

dayFromHour :: String -> UTCTime -> Maybe UTCTime
dayFromHour hourString date =
  ISO.iso8601ParseM (dateString <> "T" <> hourString <> ":00Z")
  where
    dateString = formatForDateString date
