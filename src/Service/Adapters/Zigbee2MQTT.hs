{-# LANGUAGE RecordWildCards #-}

module Service.Adapters.Zigbee2MQTT
 ( initMQTTClient
 , initZigbee2MQTTAdapter
 , mqttClientCallback
 )
where

import Control.Lens ((^.))
import Service.Adapters.Device (Device(..),  DeviceId, Devices(..), ieeeAddress, name)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode)
import Data.Either (Either(..), fromRight)
import Data.Foldable (for_)
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as M
import Data.Maybe (Maybe(..), fromMaybe, maybe, fromJust)
import Data.X509.CertificateStore (makeCertificateStore, readCertificateStore)
import Network.Connection (TLSSettings (..))
import qualified Network.MQTT.Client as MQTT
import Network.MQTT.Topic (Topic, mkFilter, mkTopic)
import Network.TLS (ClientHooks (..), ClientParams (..), Credentials (..), Shared (..), Supported (..), Version (..), credentialLoadX509, defaultParamsClient)
import Network.TLS.Extra.Cipher (ciphersuite_default)
import Network.URI (URI, parseURI)
import Prelude (Bool(..), Eq, IO, Show, String, ($), (<$>), (<>), (*), fst, putStrLn, pure, show, snd)
-- import Service.App (Logger)
-- import qualified Service.App as App
import Service.Env (LogLevel (..), MQTTConfig (..), Subscriptions)
-- import Service.MQTT.Zigbee2MQTT as Zigbee2MQTT
import UnliftIO.Async (async)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM (TChan, TVar, atomically, newTChanIO, newTVarIO, readTChan, readTVarIO, writeTChan, writeTVar)


data DeviceStore = DeviceStore
  { devices :: M.HashMap DeviceId Device
  } deriving (Eq, Show)


-- hacks while spiking

creds :: (String, String)
creds = ("automation-service-dev", "<whoops not the real key heh>")

uri :: URI
uri = fromJust $ parseURI $ "mqtts://" <> (fst creds) <> ":" <> (snd creds) <> "@mosquitto:8883"


initMQTTClient :: MQTT.MessageCallback -> MQTTConfig -> IO MQTT.MQTTClient
initMQTTClient msgCB (MQTTConfig {..}) = do
  mCertStore <- maybe (pure Nothing) readCertificateStore _caCertPath
  eCreds <- case (_clientCertPath, _clientKeyPath) of
    (Just clientCertPath', Just clientKeyPath') ->
      credentialLoadX509 clientCertPath' clientKeyPath'
    _ -> pure $ Left "clientCertPath and/or clientKeyPath are empty"

  let mqttConfig' = mkMQTTConfig $ mkClientParams eCreds mCertStore

  -- MQTT.connectURI mqttConfig' _uri
  -- substituted module uri for testing
  MQTT.connectURI mqttConfig' uri

  where
    clientParams' = defaultParamsClient "mosquitto" ""

    mkClientParams eCreds mCertStore = clientParams'
      { clientSupported =
          (clientSupported clientParams')
          { supportedVersions = [TLS13]
          , supportedCiphers = ciphersuite_default
          }
      , clientHooks =
          (clientHooks clientParams')
          { onCertificateRequest =
              fromRight (onCertificateRequest $ clientHooks clientParams') $
                clientCertificate <$> eCreds
          }
      , clientShared =
          (clientShared clientParams')
          { sharedCredentials = fromRight (sharedCredentials $ clientShared clientParams') $
              (\c -> Credentials [c]) <$> eCreds
          , sharedCAStore = fromMaybe (makeCertificateStore []) mCertStore
          }
      }

    mkMQTTConfig clientParams = MQTT.mqttConfig
      { MQTT._connID = "automation-service"
      , MQTT._tlsSettings = TLSSettings clientParams
      , MQTT._msgCB = msgCB
      }

    -- clientCertificate ::
    --   ([CertificateType], Maybe [HashAndSignatureAlgorithm], [DistinguishedName]) ->
    --   IO (Maybe (CertificateChain, PrivKey))
    --
    -- FIX ME BEFORE THIS GOES IN FOR REAL
    --
    clientCertificate cred' (certtypes, mHashSigs, dns) = do
      putStrLn $ "Implement me -- certtypes: " <> show certtypes <> ", mHashSigs: " <> show mHashSigs <> ", DNs: " <> show dns
      pure $ Just cred'


data DeviceMessage
  = DevicesUpdated
  | Quiescent


-- | Returns a SimpleCallback which is an alias for type
-- MQTTClient -> Topic -> ByteString -> [Property] -> IO ()
--
mqttClientCallback
  :: TVar DeviceStore
  -> TChan DeviceMessage
  -> TChan (Topic, ByteString)
  -> MQTT.MessageCallback
--  :: (Logger logger)
--  => LogLevel
--  -> logger
--  -> TVar Subscriptions
--  -> MQTT.MessageCallback
-- mqttClientCallback logLevelSet logger subscriptions =
mqttClientCallback deviceStore deviceUpdateChan stateUpdatesChan =
  MQTT.SimpleCallback $ \_mc topic msg _props -> do
    case topic of
      "zigbee2mqtt/bridge/devices" -> do
        putStrLn $ "devices topic:" <> show topic
        -- putStrLn $ "devices msg:" <> show msg
        -- putStrLn $ "devices parsed msg:" <> show (decode msg :: Maybe Devices)
        for_ (decode msg :: Maybe Devices) $ \devices ->
          let 
            deviceMap =
              M.fromList $
                (\device -> (device ^. ieeeAddress, device)) <$> (loadDevices devices)
          in
            atomically $ do
              writeTVar deviceStore $ DeviceStore deviceMap
              writeTChan deviceUpdateChan DevicesUpdated

      "zigbee2mqtt/bridge/groups" -> do
        putStrLn $ "groups topic:" <> show topic
        putStrLn $ "groups msg:" <> show msg
      _ -> do
        atomically $
          writeTChan stateUpdatesChan (topic, msg)
        -- putStrLn $ "Other topic: " <> show topic
        -- putStrLn $ "Other msg: " <> show msg


-- needs to load up devices and groups from MQTT on start
  -- parse into structures
-- then needs to go through each item in both collections and subscribe to updates for all

initZigbee2MQTTAdapter :: MQTTConfig -> IO ()
initZigbee2MQTTAdapter mqttConfig = do
  deviceStore <- newTVarIO $ DeviceStore M.empty

  deviceUpdateChan <- newTChanIO
  stateUpdatesChan <- newTChanIO

  mc2 <- initMQTTClient (mqttClientCallback deviceStore deviceUpdateChan stateUpdatesChan) mqttConfig

  void $ liftIO $ async $
    let
      go = do
        msg <- atomically $ readTChan deviceUpdateChan
        case msg of
          DevicesUpdated -> do
            devicesUpdated <- devices <$> readTVarIO deviceStore
            for_ devicesUpdated $ \device -> do
              let
                subTopicText = "zigbee2mqtt/" <> device ^. name
                getTopicText = subTopicText <> "/get"
              for_ (mkFilter subTopicText) $ \topic -> do
                MQTT.subscribe mc2 [(topic, MQTT.subOptions)] []
              for_ (mkTopic getTopicText) $ \topic -> do
                MQTT.publish mc2 topic "{\"state\": \"\"}" False
            go
          
          _ -> go
    in
      go

  void $ liftIO $ async $
    let
      go = do
        msg <- atomically $ readTChan stateUpdatesChan
        putStrLn $ show msg
        go
    in
      go

  threadDelay $ 1000 * 1000

  for_ ["zigbee2mqtt/bridge/devices", "zigbee2mqtt/bridge/groups"] $ \topicText ->
    for_ (mkFilter topicText) $ \topic -> do
      putStrLn $ "subscribing to topic " <> (show topicText)
      MQTT.subscribe mc2 [(topic, MQTT.subOptions)] []

  -- TODO next: sub to individual device topics from what I've loaded
  -- into schema, and then start handling device state including
  -- generating deltas, then can finally test the last step of sending
  -- commands back, mocking frontend devices
