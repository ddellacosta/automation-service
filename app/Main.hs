module Main where

import Prelude hiding (log)

import Control.Lens.Unsound (lensProduct)
import Control.Lens ((^.))
import Dhall (inputFile)
import qualified Network.MQTT.Client as MQTT
import qualified Service.App.Daemon as Daemon
import Service.App (loggerConfig, runActionsService)
import Service.Env
  ( Env
  , Env'(..)
  , actionsServiceTopicFilter
  , configDecoder
  , logLevel
  , mqttConfig
  )
import Service.MQTTClient (mqttClientCallback, initMQTTClient)
import System.Log.FastLogger (newTimedFastLogger)
import UnliftIO.STM (newTQueueIO)


-- this needs to be more intelligent, in particular in terms of how we
-- expect it to interact with Docker, if that is a main way we expect
-- folks to run this

-- TODO probably need to provide a way to configure the config file path
-- as an argument
configFilePath :: FilePath
configFilePath = "./config.dhall"

main :: IO ()
main = initialize >>= flip runActionsService Daemon.run

-- TODO this needs way better error handling
initialize :: IO Env
initialize = do
  -- need to handle a configuration error? Dhall provides a lot of error output
  config' <- inputFile configDecoder configFilePath
  (fmtTime, logType) <- loggerConfig config'

  let
    (mqttConfig', logLevelSet) = config' ^. lensProduct mqttConfig logLevel

  -- handle failure to open/write to file, anything else?
  (logger', loggerCleanup) <- newTimedFastLogger fmtTime logType
  messagesChan' <- newTQueueIO

  -- handle errors from not being able to connect, etc.?
  mc <- initMQTTClient (mqttClientCallback logLevelSet messagesChan' logger') mqttConfig'

  (_eithers, _props) <-
    MQTT.subscribe mc [(mqttConfig' ^. actionsServiceTopicFilter, MQTT.subOptions)] []

  pure $ Env' config' logger' mc messagesChan' (loggerCleanup >> MQTT.normalDisconnect mc)
