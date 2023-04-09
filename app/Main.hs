module Main where

import Prelude hiding (log)

import Control.Lens ((^.))
import Control.Lens.Unsound (lensProduct)
import qualified Network.MQTT.Client as MQTT
import Network.MQTT.Topic (toFilter)
import qualified Service.Daemon as Daemon
import qualified Service.App as App
import qualified Service.Env as Env
import Service.Env
  ( Config
  , LoggerVariant(TFLogger)
  , MQTTClientVariant(..)
  , MQTTDispatch
  , automationServiceTopic
  , logLevel
  , mqttConfig
  )
import Service.MQTT.Client (mqttClientCallback, initMQTTClient)
import Service.MQTT.Zigbee2MQTT as Zigbee2MQTT
import System.Log.FastLogger (newTimedFastLogger)
import UnliftIO.STM (TVar)


-- this needs to be more intelligent, in particular in terms of how we
-- expect it to interact with Docker, if that is a main way we expect
-- folks to run this

-- TODO probably need to provide a way to configure the config file path
-- as an argument
configFilePath :: FilePath
configFilePath = "config.dhall"

mkLogger :: Config -> IO (LoggerVariant, IO ())
mkLogger config' = do
  (fmtTime, logType) <- App.loggerConfig config'
  -- TODO handle failure to open/write to file properly
  (tfLogger, cleanup) <- newTimedFastLogger fmtTime logType
  pure (TFLogger tfLogger, cleanup)

mkMQTTClient
  :: Config -> LoggerVariant -> TVar MQTTDispatch -> IO (MQTTClientVariant, IO ())
mkMQTTClient config logger mqttDispatch = do
  let
    (mqttConfig', logLevelSet) = config ^. lensProduct mqttConfig logLevel
    mqttSubs =
      [ (toFilter $ mqttConfig' ^. automationServiceTopic, MQTT.subOptions)
      , (toFilter Zigbee2MQTT.devicesTopic, MQTT.subOptions)
      , (toFilter Zigbee2MQTT.groupsTopic, MQTT.subOptions)
      ]

  -- handle errors from not being able to connect, etc.?
  mc <- initMQTTClient (mqttClientCallback logLevelSet logger mqttDispatch) mqttConfig'
  (_eithers, _props) <- MQTT.subscribe mc mqttSubs []

  pure (MCClient $ mc, MQTT.normalDisconnect mc)


main :: IO ()
main = Env.initialize configFilePath mkLogger mkMQTTClient
  >>= flip App.runAutomationService Daemon.run
