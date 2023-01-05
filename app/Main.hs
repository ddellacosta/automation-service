{-# LANGUAGE GADTs #-}

module Main where

import Prelude hiding (log)

import Control.Lens ((^.))
import Control.Monad (when)
import Data.Aeson (decode)
import Data.Foldable (for_)
import Data.Text (Text)
import Dhall (inputFile)
import qualified Network.MQTT.Client as MQTT
import qualified Service.App.Daemon as Daemon
import Service.App (log, loggerConfig, runActionsService)
import Service.Env
  ( Config
  , Env(..)
  , LogLevel(Debug)
  , actionsServiceTopicFilter
  , configDecoder
  , logLevel
  , mqttConfig
  )
import Service.MQTTClient (initMQTTClient)
import qualified Service.Messages.Action as Messages
import System.Log.FastLogger (TimedFastLogger, newTimedFastLogger)
import UnliftIO.STM (TQueue, atomically, newTQueueIO, writeTQueue)

-- this needs to be more intelligent, in particular in terms of how we expect it to interact with Docker, if that is a main way we expect folks to run this
configFilePath :: FilePath
configFilePath = "./config.dhall"

-- TODO probably need to provide a way to configure the config file path
-- as an argument

main :: IO ()
main = initialize >>= flip runActionsService Daemon.run

-- TODO this needs way better error handling
initialize :: IO Env
initialize = do
  -- need to handle a configuration error? Dhall provides a lot of error output
  config' <- inputFile configDecoder configFilePath :: IO Config
  (fmtTime, logType) <- loggerConfig config'

  let mqttConfig' = config' ^. mqttConfig
  let logLevelSet = config' ^. logLevel

  -- handle failure to open/write to file, anything else?
  (logger', loggerCleanup) <- newTimedFastLogger fmtTime logType
  messagesChan' <- newTQueueIO

  -- handle errors from not being able to connect, etc.?
  mc <- initMQTTClient (callback logLevelSet messagesChan' logger') mqttConfig'

  (_eithers, _props) <-
    MQTT.subscribe mc [(mqttConfig' ^. actionsServiceTopicFilter, MQTT.subOptions)] []
  --  print $ show props
  --  print $ foldMap (either show show) eithers
  pure $ Env config' logger' loggerCleanup mc messagesChan'

  where
    --
    -- returns a SimpleCallback with type
    -- MQTTClient -> Topic -> ByteString -> [Property] -> IO ()
    --
    callback :: LogLevel -> TQueue (Messages.Action Text) -> TimedFastLogger -> MQTT.MessageCallback
    callback logLevelSet messagesChan' logger' =
      MQTT.SimpleCallback $ \_mc topic' msg _props -> do
        when (Debug >= logLevelSet) $
          log logger' Debug $ "Received message " <> (show msg) <> " to " <> (show topic')
        for_ (decode msg) $ atomically . writeTQueue messagesChan'
