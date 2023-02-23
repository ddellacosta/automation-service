module Test.Integration.TestApp
  ( Env
  , TestActionsService
  , testActionsService
  , initEnv
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
import Service.Env (Config, Env'(Env'), configDecoder)
import UnliftIO.STM (TVar, newTQueueIO, newTVarIO)

type LogStore = TVar [Text]
type MQTTMsgStore = TVar (M.Map Text [Text])

type Env = Env' LogStore MQTTMsgStore

newtype TestActionsService a = TestActionsService (ReaderT Env IO a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Env
    , MonadUnliftIO
    )

-- TODO store values in LogStore for checking test results
instance App.Logger TestActionsService where
  debug = const $ pure ()
  info = const $ pure ()
  warn =  const $ pure ()
  error = const $ pure ()

-- TODO store values in MQTTMsgStore for checking test results
instance App.MonadMQTT TestActionsService where
  publishMQTT _topic _msg = undefined

testActionsService :: Env -> TestActionsService a -> IO a
testActionsService env (TestActionsService x) = runReaderT x env

testConfigFilePath :: FilePath
testConfigFilePath = "./test/config.dhall"

initEnv :: IO Env
initEnv = do
  config' <- inputFile configDecoder testConfigFilePath :: IO Config
  logger <- newTVarIO []
  mc <- newTVarIO M.empty
  messagesChan' <- newTQueueIO
  devices <- newTVarIO []
  deviceMessagesChan <- newTQueueIO
  pure $
    Env' config' logger mc messagesChan' (pure ()) devices deviceMessagesChan
