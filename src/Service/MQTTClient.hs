{-# LANGUAGE RecordWildCards #-}

module Service.MQTTClient
  ( initMQTTClient
  )
where

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
import Service.Env (MQTTConfig(..))

initMQTTClient :: MQTT.MessageCallback -> MQTTConfig -> IO MQTT.MQTTClient
initMQTTClient msgCB (MQTTConfig {..}) = do
  mCertStore <- readCertificateStore _caCertPath
  eCreds <- credentialLoadX509 _clientCertPath _clientKeyPath

  let mqttConfig' = (mkMQTTConfig $ mkClientParams eCreds mCertStore)

  MQTT.connectURI mqttConfig' _uri 

  where
    certStore = fromMaybe (makeCertificateStore [])

    cred = either (\i -> error $ "got a bad credential! " <> i) id

    clientParams' = defaultParamsClient "mosquitto" ""

    mkClientParams eCreds mCertStore = clientParams'
      { clientSupported =
          (clientSupported clientParams')
          { supportedVersions = [TLS13]
          , supportedCiphers = ciphersuite_default
          }
      , clientHooks =
          (clientHooks clientParams')
          { onCertificateRequest = clientCertificate (cred eCreds)
          }
      , clientShared =
          (clientShared clientParams')
          { sharedCredentials = Credentials [(cred eCreds)]
          , sharedCAStore = certStore mCertStore
          }
      }

    mkMQTTConfig clientParams = MQTT.mqttConfig
      { MQTT._connID = "actions-service"
      , MQTT._tlsSettings = TLSSettings clientParams
      , MQTT._msgCB = msgCB
      }

    -- clientCertificate ::
    --   ([CertificateType], Maybe [HashAndSignatureAlgorithm], [DistinguishedName]) ->
    --   IO (Maybe (CertificateChain, PrivKey))
    clientCertificate cred' (certtypes, mHashSigs, dns) = do
      putStrLn $ "Implement me -- certtypes: " <> show certtypes <> ", mHashSigs: " <> show mHashSigs <> ", DNs: " <> show dns
      pure $ Just cred'
