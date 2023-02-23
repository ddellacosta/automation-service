{-# LANGUAGE RecordWildCards #-}

module Service.MQTTClient
  ( initMQTTClient
  , mqttClientCallback
  )
where

import Control.Monad (when)
import Data.Aeson (decode)
import Data.Either (fromRight)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.X509.CertificateStore (makeCertificateStore, readCertificateStore)
import Network.Connection (TLSSettings(..))
import qualified Network.MQTT.Client as MQTT
import Network.TLS
  ( ClientHooks(..)
  , ClientParams(..)
  , Credentials(..)
  , Shared(..)
  , Supported(..)
  , Version(..)
  , credentialLoadX509
  , defaultParamsClient
  )
import Network.TLS.Extra.Cipher (ciphersuite_default)
import qualified Service.App as App
import Service.Env (MQTTConfig(..), LogLevel(..))
import qualified Service.Messages.Daemon as Daemon
import qualified Service.Messages.Zigbee2MQTTDevice as Zigbee2MQTTDevice
import System.Log.FastLogger (TimedFastLogger)
import UnliftIO.STM (TQueue, atomically, writeTQueue)


initMQTTClient :: MQTT.MessageCallback -> MQTTConfig -> IO MQTT.MQTTClient
initMQTTClient msgCB (MQTTConfig {..}) = do
  mCertStore <- maybe (pure Nothing) readCertificateStore _caCertPath
  eCreds <- case (_clientCertPath, _clientKeyPath) of
    (Just clientCertPath', Just clientKeyPath') ->
      credentialLoadX509 clientCertPath' clientKeyPath'
    _ -> pure $ Left "clientCertPath and/or clientKeyPath are empty"

  let mqttConfig' = mkMQTTConfig $ mkClientParams eCreds mCertStore

  MQTT.connectURI mqttConfig' _uri 

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
    clientCertificate cred' (certtypes, mHashSigs, dns) = do
      putStrLn $ "Implement me -- certtypes: " <> show certtypes <> ", mHashSigs: " <> show mHashSigs <> ", DNs: " <> show dns
      pure $ Just cred'


-- | Returns a SimpleCallback which is an alias for type
-- MQTTClient -> Topic -> ByteString -> [Property] -> IO ()
--
mqttClientCallback
  :: LogLevel
  -> TimedFastLogger
  -> TQueue Daemon.Message
  -> TQueue Zigbee2MQTTDevice.Message
  -> MQTT.MessageCallback
mqttClientCallback logLevelSet logger' messagesQueue deviceMessageQueue =
  MQTT.SimpleCallback $ \_mc topic msg _props -> do
    when (Debug >= logLevelSet) $
      App.log logger' Debug $ "Received message " <> show msg <> " to " <> show topic

    let
      write ::TQueue a -> (a -> IO ())
      write tq = atomically . writeTQueue tq

    if (topic /=  "zigbee2mqtt/bridge/devices")
    then
      for_ (decode msg) $ write messagesQueue
    else
      for_ (decode msg) $ write deviceMessageQueue
