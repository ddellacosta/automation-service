{-# LANGUAGE RecordWildCards #-}

module Service.MQTT.Client
  ( initMQTTClient
  , mqttClientCallback
  )
where

import Control.Monad (when)
import Data.Either (fromRight)
import qualified Data.HashMap.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.X509.CertificateStore (makeCertificateStore, readCertificateStore)
import Network.Connection (TLSSettings (..))
import qualified Network.MQTT.Client as MQTT
import Network.TLS (ClientHooks (..), ClientParams (..), Credentials (..), Shared (..),
                    Supported (..), Version (..), credentialLoadX509, defaultParamsClient)
import Network.TLS.Extra.Cipher (ciphersuite_default)
import qualified Service.App as App
import Service.App (Logger)
import Service.Env (LogLevel (..), MQTTConfig (..), Subscriptions)
import UnliftIO.STM (TVar, readTVarIO)


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
    --
    -- TODO
    --
    -- With TLS it is also desirable that a client connecting to a
    -- server is able to validate ownership of the server’s public
    -- key. This is normally undertaken using an X.509 digital
    -- certificate issued by a trusted third party known as a
    -- Certificate Authority (CA) which asserts the authenticity of
    -- the public key. In some cases, a server may use a self-signed
    -- certificate which needs to be explicitly trusted by the client
    -- (browsers should display a warning when an untrusted
    -- certificate is encountered), but this may be acceptable in
    -- private networks and/or where secure certificate distribution
    -- is possible. It is highly recommended though, to use
    -- certificates issued by publicly trusted CAs.
    --
    -- at the bottom of "How does TLS work?"
    -- https://www.internetsociety.org/deploy360/tls/basics/
    --
    -- The guidance I'm taking from that is that the correct thing to
    -- do is probably to ensure that the certificate matches one in
    -- the public record somehow, or otherwise is registered (whatever
    -- that might mean if I have to implement this explicitly or there
    -- is something built-in to ?) to Data.X509.CertificateStore or
    -- etc. to help me this server (in the case of a self-signed cert).
    --
    clientCertificate cred' (certtypes, mHashSigs, dns) = do
      putStrLn $ "Implement me -- certtypes: " <> show certtypes <> ", mHashSigs: " <> show mHashSigs <> ", DNs: " <> show dns
      pure $ Just cred'


-- | Returns a SimpleCallback which is an alias for type
-- MQTTClient -> Topic -> ByteString -> [Property] -> IO ()
--
mqttClientCallback
  :: (Logger logger)
  => LogLevel
  -> logger
  -> TVar Subscriptions
  -> MQTT.MessageCallback
mqttClientCallback logLevelSet logger subscriptions =
  MQTT.SimpleCallback $ \_mc topic msg _props -> do
    when (Debug >= logLevelSet) $
      App.log logger Debug $
        "Received message " <> T.pack (show msg) <> " to " <> T.pack (show topic)
    subscriptions' <- readTVarIO subscriptions
    case M.lookup topic subscriptions' of
      Just msgActions -> mapM_ (\action -> action topic msg) $ M.elems msgActions
      Nothing -> do
        when (Warn >= logLevelSet) $
          App.log logger Warn $
            "Received message " <>
            T.pack (show msg) <>
            " to unhandled topic " <>
            T.pack (show topic)
