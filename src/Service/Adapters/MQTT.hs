{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Service.Adapters.MQTT
 ( Captures(..)
 , TopicCallbackArgs(..)
 , TopicCallbacks(..)
 , initMQTTClient
 , mqttClientCallback
 )
where

import Control.Lens (makeClassyPrisms) --, makeFieldsNoPrefix)
import Data.ByteString.Lazy (ByteString)
import Data.Coerce (coerce)
import Data.Either (fromRight)
import Data.Foldable (for_)
import qualified Data.HashMap.Strict as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Semigroup ()
import qualified Data.Text as T
import Data.X509.CertificateStore (makeCertificateStore, readCertificateStore)
import Network.Connection (TLSSettings (..))
import qualified Network.MQTT.Client as MQTT
import Network.MQTT.Topic (Topic)
import Network.TLS (ClientHooks (..), ClientParams (..), Credentials (..), Shared (..), Supported (..), Version (..), credentialLoadX509, defaultParamsClient)
import Network.TLS.Extra.Cipher (ciphersuite_default)
import Network.URI (URI, parseURI)
import Service.Adapters.DeviceStore (DeviceStore, DeviceMessage, FrontendMessage)
import Service.Env (MQTTConfig(..))
import UnliftIO.STM (TChan, TVar)

-- hacks while spiking

creds :: (String, String)
creds = ("automation-service-dev", "")

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
      { MQTT._connID = "automation-service-dev"
      , MQTT._tlsSettings = TLSSettings clientParams
      , MQTT._msgCB = msgCB
      }

    -- clientCertificate ::
    --   ([CertificateType], Maybe [HashAndSignatureAlgorithm], [DistinguishedName]) ->
    --   IO (Maybe (CertificateChain, PrivKey))
    --
    -- FIX ME BEFORE THIS GOES IN FOR REAL
    --
    clientCertificate cred' (_certtypes, _mHashSigs, _dns) = do
      -- putStrLn $ "Implement me -- certtypes: " <> show certtypes <> ", mHashSigs: " <> show mHashSigs <> ", DNs: " <> show dns
      pure $ Just cred'


data Captures
  -- = ESPHomeCaptures (M.HashMap T.Text T.Text)
  = ESPHomeCaptures Topic
  | Zigbee2MQTTCaptures Topic
  deriving (Show)
  
makeClassyPrisms ''Captures


data TopicCallbackArgs
  = TopicCallbackArgs
  { topic :: Topic
  , msg :: ByteString
  , deviceStore :: TVar DeviceStore
  , deviceUpdatesChan :: TChan DeviceMessage
  , frontendUpdatesChan :: TChan FrontendMessage
  }

type TopicCallbackList = 
  [(TopicCallbackArgs -> Topic -> IO (Maybe Captures), TopicCallbackArgs -> Captures -> IO ())]

newtype TopicCallbacks = TopicCallbacks TopicCallbackList
  deriving (Semigroup)

-- -- each adapter: polymorphic in c, takes an injection
-- esphomeCallbacks :: (EsphomeTopic -> c) -> TopicCallbacks c
-- zigbeeCallbacks  :: (ZigbeeTopic  -> c) -> TopicCallbacks c
-- 
-- -- composition root: the only module that knows both
-- callbacks = esphomeCallbacks ESPHomeCaptures <> zigbeeCallbacks Zigbee2MQTTCaptures


-- | Returns a SimpleCallback which is an alias for type
-- MQTTClient -> Topic -> ByteString -> [Property] -> IO ()
--
mqttClientCallback
  :: TVar DeviceStore
  -> TChan DeviceMessage
  -> TChan FrontendMessage
  -> TopicCallbacks
  -> MQTT.MessageCallback
mqttClientCallback deviceStore deviceUpdatesChan frontendUpdatesChan topicCallbacks =
  MQTT.SimpleCallback $ \_mc topic msg _props -> do
    let
      topicCallbacks' = coerce topicCallbacks :: TopicCallbackList
      topicCallbackArgs =
        TopicCallbackArgs
          topic msg deviceStore deviceUpdatesChan frontendUpdatesChan

    for_ topicCallbacks' $ \(topicPred, handler) -> do
      mTopicCaptures <- topicPred topicCallbackArgs topic
      for_ mTopicCaptures $ handler topicCallbackArgs
