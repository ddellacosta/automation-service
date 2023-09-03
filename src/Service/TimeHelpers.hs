{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Service.TimeHelpers
  ( Coordinates
  , SunEvent
  , SunEvents
  , Sunrise
  , Sunset
  , addMinutes
  , getSunriseAndSunset
  , getTimeZone
  , getZonedTimeForUTC
  , minuteDiff
  , utcTimeToCronInstant
  )
where

import Prelude hiding (min)

import Control.Exception (IOException, handle)
import Control.Lens (_1, _Just, preview)
import qualified Data.Astro.Sun as AstroSun
import qualified Data.Astro.Time.Conv as AstroConv
import qualified Data.Astro.Types as AstroTypes
import Data.Fixed (Pico)
import Data.Time.Clock (NominalDiffTime, UTCTime)
import qualified Data.Time.Clock as C
import qualified Data.Time.Format as F
import Data.Time.LocalTime (TimeZone, ZonedTime)
import qualified Data.Time.LocalTime as LT
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

-- | Given a UTCTime, returns the timezone, calculated using the
-- environment variable "TZ" exclusively.
--
-- This was necessitated by getting inconsistent results between local
-- testing and in the context of a docker container when using
-- Data.Time.LocalTime's getCurrentTimeZone or getTimeZone. As a
-- result I abandoned efforts to configure my docker container's linux
-- instance and instead opted to calculate the timezone internally
-- based on the value of the TZ env. var, as I was able to
-- consistently confirm that was being set successfully.
--
getTimeZone :: UTCTime -> IO TimeZone
getTimeZone ts = do
  tzVar <- handle (\(_e :: IOException) -> pure "UTC") $ getEnv "TZ"
  -- need to handle this failing too
  tz <- Z.loadTZFromDB tzVar
  pure $ Z.timeZoneForUTCTime tz ts

-- | Given a UTCTime, converts it into a ZonedTime using the current
-- TZ env. var setting per getTimeZone.
--
getZonedTimeForUTC :: UTCTime -> IO ZonedTime
getZonedTimeForUTC ts = do
  tz <- getTimeZone ts
  pure $ LT.utcToZonedTime tz ts

type Coordinates = (Double, Double)

type Sunrise = Maybe UTCTime
type Sunset = Maybe UTCTime
type SunEvent = Maybe UTCTime
type SunEvents = (Sunrise, Sunset)

-- | Takes a UTCTime representing the day we want sun events
-- for--whatever that translates to in the local timezone, meaning
-- what is explicitly set to the environment var *TZ--and a set of
-- latitude/longitude coordinates in decimal format, and returns a
-- sunrise/sunset pair, each value wrapped in a Maybe.
--
-- *(Due to a number of issues getting reliable results configuring
-- timezone to work with time as expected, all calculations to
-- calculate the timezone is handled internally. See getTimeZone.)
--
getSunriseAndSunset :: UTCTime -> Coordinates -> IO SunEvents
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
    getNextDaySunset :: UTCTime -> Coordinates -> IO SunEvent
    getNextDaySunset ts' (lat', long') = do
      nextDay <- getZonedTimeForUTC . addMinutes 1440 $ ts'
      let
        (_sunrise, sunset) = getSunriseAndSunset' nextDay (lat', long')
      pure sunset

getSunriseAndSunset' :: ZonedTime -> Coordinates -> SunEvents
getSunriseAndSunset' ts (lat, long) =
  (toUTC sunrise, toUTC sunset)

  where
    coords =
      AstroTypes.GeoC (AstroTypes.DD lat) (AstroTypes.DD long)

    -- see docs for sunRiseAndSet:
    verticalShift = (AstroTypes.DD 0.833333)

    lcd = AstroConv.zonedTimeToLCD ts

    -- fno-warn-incomplete-uni-patterns at the top is for this
    (AstroSun.RiseSet sunrise sunset) =
      AstroSun.sunRiseAndSet coords verticalShift lcd

    toUTC sunEvent =
      LT.zonedTimeToUTC . AstroConv.lctToZonedTime <$> preview (_Just . _1) sunEvent
