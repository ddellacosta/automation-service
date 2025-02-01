module AutomationService.Logging
  ( LogLevel(..)
  , debug
  , deserializeLogLevel
  , error
  , getLogLevel
  , info
  , log
  , serializeLogLevel
  , setLogLevel
  , warn
  )
where

import Data.Eq (class Eq)
import Data.Maybe (Maybe(..))
import Data.Ord (class Ord)
import Effect (Effect)
import Effect.Console as Console
import Prelude (Unit, ($), (>=), bind, pure, when)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

data LogLevel
  = Debug
  | Info
  | Warn
  | Error

derive instance Eq LogLevel
derive instance Ord LogLevel

debug :: String -> Effect Unit
debug msg = log Debug msg

info :: String -> Effect Unit
info msg = log Info msg

warn :: String -> Effect Unit
warn msg = log Warn msg

error :: String -> Effect Unit
error msg = log Error msg

getLogLevel :: Effect LogLevel
getLogLevel = do
  w <- window
  s <- localStorage w
  level <- getItem "LogLevel" s
  pure $ case level of
    Just level' -> deserializeLogLevel level'
    Nothing -> Info

setLogLevel :: LogLevel -> Effect Unit
setLogLevel level = do
  w <- window
  s <- localStorage w
  setItem "LogLevel" (serializeLogLevel level) s

serializeLogLevel :: LogLevel -> String
serializeLogLevel = case _ of
  Debug -> "debug"
  Info -> "info"
  Warn -> "warn"
  Error -> "error"

deserializeLogLevel :: String -> LogLevel
deserializeLogLevel = case _ of
  "debug" -> Debug
  "info" -> Info
  "warn" -> Warn
  "error" -> Error
  _ -> Info

log :: LogLevel -> String -> Effect Unit
log level msg = do
  configuredLevel <- getLogLevel
  when (level >= configuredLevel)
    case level of
      Debug -> Console.debug msg
      Info -> Console.info msg
      Warn -> Console.warn msg
      Error -> Console.error msg
