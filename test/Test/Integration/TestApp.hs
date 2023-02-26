module Test.Integration.TestApp
  ( initEnv
  )
where

import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Dhall (inputFile)
import qualified Network.MQTT.Client as MQTT
import Network.MQTT.Client (MessageCallback(SimpleCallback), MQTTConfig(_msgCB))
import Network.URI (parseURI)
import qualified Service.App as App
import Service.Env (Config, Env(Env), LoggerVariant(..), MQTTClientVariant(..), configDecoder)
import Test.Helpers (loadTestDevices)
import UnliftIO.STM (TVar, newTQueueIO, newTVarIO)

testConfigFilePath :: FilePath
testConfigFilePath = "./test/config.dhall"

initEnv :: IO Env
initEnv = do
  config' <- inputFile configDecoder testConfigFilePath :: IO Config
  logger <- newTVarIO []
  mc <- newTVarIO M.empty
  messagesChan' <- newTQueueIO
  testDevices <- loadTestDevices
  devices <- newTVarIO testDevices
  pure $
    Env config' (TQLogger logger) (TQClient mc) messagesChan' (pure ()) devices
