module Test.Integration.TestApp
  ( testActionsService
  , initEnv
  )
where

import Control.Lens ((^.))
import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Dhall (inputFile)
import qualified Network.MQTT.Client as MQTT
import Network.MQTT.Client (MessageCallback(SimpleCallback), MQTTConfig(_msgCB))
import Network.URI (parseURI)
import Service.App (loggerConfig)
import qualified Service.App as App
import Service.Env (Config, Env(Env), configDecoder, mqttConfig)
import Service.MQTTClient (initMQTTClient)
import System.Log.FastLogger (newTimedFastLogger)
import UnliftIO.STM (newTQueueIO)

newtype TestActionsService a = TestActionsService (ReaderT Env IO a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Env
    , MonadUnliftIO
    )

instance App.Logger TestActionsService where
  debug = const $ pure ()
  info = const $ pure ()
  warn =  const $ pure ()
  error = const $ pure ()

instance App.MonadMQTT TestActionsService where
  publishMQTT _topic _msg = undefined

testActionsService :: Env -> TestActionsService a -> IO a
testActionsService env (TestActionsService x) = runReaderT x env

testConfigFilePath :: FilePath
testConfigFilePath = "./test/config.dhall"

initEnv :: IO Env
initEnv = do
  config' <- inputFile configDecoder testConfigFilePath :: IO Config
  (fmtTime, logType) <- loggerConfig config'
  (logger', loggerCleanup) <- newTimedFastLogger fmtTime logType
  mc <- initMQTTClient mqttCallback (config' ^. mqttConfig)
  messagesChan' <- newTQueueIO
  pure $ Env config' logger' (loggerCleanup >> MQTT.normalDisconnect mc) mc messagesChan'

  where
    -- mqttCallback :: MQTTClient -> Topic -> ByteString -> [Property] -> IO ()
    mqttCallback = MQTT.SimpleCallback $ \_mc topic _msg _props -> do
      putStrLn $ show topic

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