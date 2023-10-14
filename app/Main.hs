module Main where

import Prelude hiding (log)

import Control.Lens ((^.))
import Control.Lens.Unsound (lensProduct)
import qualified Data.HashMap.Strict as M
import qualified Network.MQTT.Client as MQTT
import Network.MQTT.Topic (toFilter)
import qualified Service.App as App
import Service.App (Logger)
import qualified Service.Daemon as Daemon
import qualified Service.Env as Env
import Service.Env (Config, MQTTDispatch, logLevel, mqttConfig)
import Service.MQTT.Client (initMQTTClient, mqttClientCallback)
import System.Log.FastLogger (TimedFastLogger, newTimedFastLogger)
import UnliftIO.STM (TVar, readTVarIO)


-- this needs to be more intelligent, in particular in terms of how we
-- expect it to interact with Docker, if that is a main way we expect
-- folks to run this

-- TODO probably need to provide a way to configure the config file path
-- as an argument
configFilePath :: FilePath
configFilePath = "config.dhall"

mkLogger :: Config -> IO (TimedFastLogger, IO ())
mkLogger config' = do
  (fmtTime, logType) <- App.loggerConfig config'
  -- TODO handle failure to open/write to file properly
  (tfLogger, cleanup) <- newTimedFastLogger fmtTime logType
  pure (tfLogger, cleanup)

mkMQTTClient
  :: (Logger logger)
  => Config
  -> logger
  -> TVar MQTTDispatch
  -> IO (MQTT.MQTTClient, IO ())
mkMQTTClient config logger mqttDispatch = do
  mqttDispatch' <- readTVarIO mqttDispatch

  let
    (mqttConfig', logLevelSet) = config ^. lensProduct mqttConfig logLevel
    mqttSubs = flip M.foldMapWithKey mqttDispatch' $ \topic _action ->
      [(toFilter topic, MQTT.subOptions)]

  -- handle errors from not being able to connect, etc.?
  mc <- initMQTTClient (mqttClientCallback logLevelSet logger mqttDispatch) mqttConfig'
  (_eithers, _props) <- MQTT.subscribe mc mqttSubs []

  pure (mc, MQTT.normalDisconnect mc)


main :: IO ()
main = Env.initialize configFilePath mkLogger mkMQTTClient
  >>= flip App.runAutomationService Daemon.run
