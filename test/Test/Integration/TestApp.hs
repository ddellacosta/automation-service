{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-incomplete-uni-patterns #-}

module Test.Integration.TestApp
  ( Env
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
import UnliftIO.STM (TVar, newTVarIO, newTQueueIO)

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
  pure $ Env' config' logger mc messagesChan' (pure ())

testMosquitto :: IO ()
testMosquitto = do
  let (Just uri') = parseURI "mqtt://localhost"
      subscriptions =
        [ ("tmp/topic1", MQTT.subOptions)
        , ("tmp/topic2", MQTT.subOptions)
        ]

  mc <- MQTT.connectURI MQTT.mqttConfig{_msgCB=SimpleCallback msgReceived} uri'

  MQTT.publish mc "tmp/topic1" "hello!" False

  print =<< MQTT.subscribe mc subscriptions []
  MQTT.waitForClient mc   -- wait for the the client to disconnect

  where
    msgReceived _ t m p = print (t,m,p)
