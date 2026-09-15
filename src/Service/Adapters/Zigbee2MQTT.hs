{-# LANGUAGE RecordWildCards #-}

module Service.Adapters.Zigbee2MQTT
 ( initMQTTClient
 , initZigbee2MQTTAdapter
 , mqttClientCallback
 )
where

import Control.Applicative (Applicative)
import Control.Lens (LensLike', (^.), (^?), filtered, folded, ix, view)
import Control.Monad (when)
import Service.Adapters.Capability (Kind, kind, property, valueOff, valueOn)
import Service.Adapters.Device (Device(..), DeviceId, Devices(..), capabilities, ieeeAddress, name)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value(..), (.:), decode, withObject)
import Data.Aeson.Key (toText)
import Data.Aeson.KeyMap (foldMapWithKey)
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy (ByteString)
import Data.Either (Either(..), fromRight)
import Data.Foldable (for_)
import Data.Functor.Contravariant (Contravariant)
import qualified Data.HashMap.Strict as M
import Data.Maybe (Maybe(..), fromMaybe, maybe, fromJust)
import Data.X509.CertificateStore (makeCertificateStore, readCertificateStore)
import Network.Connection (TLSSettings (..))
import qualified Network.MQTT.Client as MQTT
import Network.MQTT.Topic (mkFilter, mkTopic)
import Network.TLS (ClientHooks (..), ClientParams (..), Credentials (..), Shared (..), Supported (..), Version (..), credentialLoadX509, defaultParamsClient)
import Network.TLS.Extra.Cipher (ciphersuite_default)
import Network.URI (URI, parseURI)
import Prelude (Bool(..), Eq, IO, Show, String, (.), ($), (<$>), (=<<), (<>), (*), (>), (==), (/=), (&&), filter, fst, not, null, otherwise, putStrLn, pure, show, snd)
-- import Service.App (Logger)
-- import qualified Service.App as App
import Service.Env (LogLevel (..), MQTTConfig (..), Subscriptions)
-- import Service.MQTT.Zigbee2MQTT as Zigbee2MQTT
import UnliftIO.Async (async)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM (TChan, TVar, atomically, newTChanIO, newTVarIO, readTChan, readTVar, readTVarIO, writeTChan, writeTVar)

import qualified Data.Text as T
import Text.Pretty.Simple (pPrintLightBg)

type DeviceState = M.HashMap PropertyAddress Value

data DeviceStore = DeviceStore
  { devices :: M.HashMap DeviceId Device
  , deviceState :: M.HashMap DeviceId DeviceState
  } deriving (Eq, Show)

type DeltaBatch = (DeviceId, [(PropertyAddress, Value)])

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


calculateDeltas
  :: DeviceState -- M.HashMap PropertyAddress Value
  -> [ (PropertyAddress, Value) ]
  -> [ (PropertyAddress, Value) ]
calculateDeltas currentState =
  filter (\(prop, val) -> M.lookup prop currentState /= Just val)


-- | Returns a SimpleCallback which is an alias for type
-- MQTTClient -> Topic -> ByteString -> [Property] -> IO ()
--
mqttClientCallback
  :: TVar DeviceStore
  -> TChan DeviceMessage
  -> TChan DeltaBatch
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
              deviceStore' <- readTVar deviceStore
              -- should we do some cleanup of the deviceStore here
              -- based on new state schema we pull? Remove
              -- non-existent devices/capabilities/now-disabled
              -- devices, etc.?
              writeTVar deviceStore $ DeviceStore deviceMap (deviceState deviceStore')
              writeTChan deviceUpdateChan DevicesUpdated

      "zigbee2mqtt/bridge/groups" -> do
        putStrLn $ "groups topic:" <> show topic
        putStrLn $ "groups msg:" <> show msg
      _ -> do
        -- this needs to filter based on whether or not this is a
        -- legit device state update message
        atomically $ do
          deviceStore' <- readTVar deviceStore
          (deviceId', deltas) <-
            case decodeStateUpdate (devices deviceStore') msg of
              Just stateUpdate -> do
                let
                  deviceState' =
                    fromMaybe M.empty $
                      M.lookup (deviceId stateUpdate) (deviceState deviceStore')
                  deltas =
                    calculateDeltas deviceState' (stateValues stateUpdate)

                writeTVar
                  deviceStore
                  deviceStore' {
                    deviceState =
                      M.insert
                       (deviceId stateUpdate)
                       (M.fromList $ stateValues stateUpdate)
                       (deviceState deviceStore')
                    }

                pure (deviceId stateUpdate, deltas)

              Nothing ->
                -- ugly
                pure ("", [])

          when (not . null $ deltas) $
            writeTChan stateUpdatesChan (deviceId', deltas)
        -- putStrLn $ "Other topic: " <> show topic
        -- putStrLn $ "Other msg: " <> show msg

-- check that the state has changed or not
-- if entry exists
--   diff against the state update
--   check each property against existing property, this produces real deltas
-- else add entry

type PropertyAddress = T.Text

data StateUpdate = StateUpdate
  { deviceId :: DeviceId
  , stateValues :: [ (PropertyAddress, Value) ]
  } deriving (Eq, Show)

decodeStateUpdate :: M.HashMap DeviceId Device -> ByteString -> Maybe StateUpdate
decodeStateUpdate devices stateUpdateStr =
  parseMaybe parseStateUpdate =<< decode stateUpdateStr
  where
    parseStateUpdate = withObject "StateUpdate" $ \su -> do
      device <- su .: "device"
      deviceId <- device .: "ieeeAddr"

      let
        prepend :: T.Text -> T.Text -> T.Text
        prepend prefix key =
          if T.length prefix > 0
          then
            prefix <> "." <> key
          else
            key

        -- probably should make this a Device/Capability module lookup function(s)?
        kindLens
          :: (Applicative f, Contravariant f)
          => T.Text
          -> LensLike' f (M.HashMap DeviceId Device) Kind
        kindLens k =
          ix deviceId . capabilities . folded . filtered ((== k) . view property) . kind

        normalizeState :: Value -> M.HashMap DeviceId Device -> Value
        normalizeState v devices' =
          case v of
            String stateTxt
              | devices' ^? kindLens "state" . valueOn == Just stateTxt ->
                Bool True
              | devices' ^? kindLens "state" . valueOff == Just stateTxt ->
                Bool False
              | otherwise ->
                Bool False -- er I guess

            -- will this happen?
            Bool stateBool ->
              Bool stateBool

            _ ->
              Bool False

        stateValues prefix = foldMapWithKey $ \k v ->
          case v of
            Object o ->
              let
                keyTxt = toText k
              in
                if keyTxt /= "device" && keyTxt /= "update" then
                  stateValues (prepend prefix $ toText k) o
                else
                  []
            _ ->
              if toText k == "state" then
                [(prepend prefix $ toText k, (normalizeState v devices))]
              else
                [(prepend prefix $ toText k, v)]

      pure $ StateUpdate deviceId $ stateValues "" su


initZigbee2MQTTAdapter :: MQTTConfig -> IO ()
initZigbee2MQTTAdapter mqttConfig = do
  deviceStore <- newTVarIO $ DeviceStore M.empty M.empty

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
        (deviceId, stateUpdate) <- atomically $ readTChan stateUpdatesChan
        pPrintLightBg deviceId
        pPrintLightBg stateUpdate
        go
    in
      go

  threadDelay $ 1000 * 1000

  for_ ["zigbee2mqtt/bridge/devices", "zigbee2mqtt/bridge/groups"] $ \topicText ->
    for_ (mkFilter topicText) $ \topic -> do
      putStrLn $ "subscribing to topic " <> (show topicText)
      MQTT.subscribe mc2 [(topic, MQTT.subOptions)] []

  -- TODO next: test sending commands back, mocking frontend devices,
  -- dig into adapter architecture a bit more
