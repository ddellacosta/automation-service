module Main where

import Prelude hiding (log)

import Control.Lens ((^.))
import Control.Lens.Unsound (lensProduct)
import Dhall (inputFile)
import qualified Network.MQTT.Client as MQTT
import Network.MQTT.Topic (toFilter)
import qualified Service.App.Daemon as Daemon
import Service.App (loggerConfig, runAutomationService)
import Service.Env
  ( Env(..)
  , LoggerVariant(..)
  , MQTTClientVariant(..)
  , automationServiceTopicFilter
  , configDecoder
  , logLevel
  , mqttConfig
  )
import Service.Messages.Zigbee2MQTTDevice as Zigbee2MQTTDevice
import Service.MQTTClient (mqttClientCallback, initMQTTClient)
import System.Log.FastLogger (newTimedFastLogger)
import UnliftIO.STM (newTQueueIO, newTVarIO)


-- this needs to be more intelligent, in particular in terms of how we
-- expect it to interact with Docker, if that is a main way we expect
-- folks to run this

-- TODO probably need to provide a way to configure the config file path
-- as an argument
configFilePath :: FilePath
configFilePath = "./config.dhall"

main :: IO ()
main = initialize >>= flip runAutomationService Daemon.run

-- TODO this needs way better error handling
initialize :: IO Env
initialize = do
  -- need to handle a configuration error? Dhall provides a lot of error output
  config <- inputFile configDecoder configFilePath
  (fmtTime, logType) <- loggerConfig config

  let
    (mqttConfig', logLevelSet) = config ^. lensProduct mqttConfig logLevel
    mqttSubs =
      [ (mqttConfig' ^. automationServiceTopicFilter, MQTT.subOptions)
      , (toFilter Zigbee2MQTTDevice.topic, MQTT.subOptions)
      ]

  -- handle failure to open/write to file, anything else?
  (logger, loggerCleanup) <- newTimedFastLogger fmtTime logType
  messageQueue <- newTQueueIO
  devices <- newTVarIO []

  -- handle errors from not being able to connect, etc.?
  mc <- initMQTTClient (mqttClientCallback logLevelSet logger messageQueue) mqttConfig'
  (_eithers, _props) <- MQTT.subscribe mc mqttSubs []

  pure $
    Env
      config
      (TFLogger logger)
      (MCClient mc)
      messageQueue
      (loggerCleanup >> MQTT.normalDisconnect mc)
      devices
