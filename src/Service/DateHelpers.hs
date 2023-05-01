module Service.DateHelpers
  ( addMinutes
  , getFormattedLocalDate
  , localTimeToCronInstant
  , minuteDiff
  , parseUTCTime
  , toLocalTime
  )
where

import Data.Fixed (Pico)
import Data.Functor ((<&>))
import qualified Data.Time.Clock as C
import Data.Time.Clock (NominalDiffTime, UTCTime)
import qualified Data.Time.Format as F
import qualified Data.Time.Format.ISO8601 as ISO
import qualified Data.Time.LocalTime as LT
import Data.Time.LocalTime (LocalTime(..))

parseUTCTime :: String -> Maybe UTCTime
parseUTCTime = ISO.iso8601ParseM

minuteDiff :: Pico -> NominalDiffTime
minuteDiff = C.secondsToNominalDiffTime . (* 60)

toLocalTime :: UTCTime -> IO LocalTime
toLocalTime t = LT.getCurrentTimeZone <&> flip LT.utcToLocalTime t

localTimeToCronInstant :: LocalTime -> String
localTimeToCronInstant =
  F.formatTime F.defaultTimeLocale "%M %H %d %m %w"

addMinutes :: Pico -> UTCTime -> UTCTime
addMinutes = C.addUTCTime . minuteDiff

getFormattedLocalDate :: IO String
getFormattedLocalDate = do
  utc <- C.getCurrentTime
  F.formatTime F.defaultTimeLocale "%Y-%m-%d" <$> toLocalTime utc
