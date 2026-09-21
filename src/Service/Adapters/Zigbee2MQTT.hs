{-# LANGUAGE RecordWildCards #-}

module Service.Adapters.Zigbee2MQTT
 ( initMQTTClient
 , initZigbee2MQTTAdapter
 , mqttClientCallback
 )
where

import System.IO (BufferMode(..), hSetBuffering, stdout)
import qualified Data.ByteString.Lazy.Char8 as LBS8

import Control.Applicative (Applicative)
import Control.Lens (LensLike', (^.), (^?), filtered, folded, ix, view)
import Control.Monad (when)
import Service.Adapters.Capability (Kind, kind, property, valueOff, valueOn)
import Service.Adapters.Device (Device(..), DeviceId, Devices(..), capabilities, ieeeAddress, name)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(..), Value(..), (.:), decode, defaultOptions, encode, genericToEncoding, withObject)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Key (fromText, toText)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.KeyMap (foldMapWithKey)
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy (ByteString)
import Data.Either (Either(..), fromRight)
import Data.Foldable (fold, foldl', for_)
import Data.Functor.Contravariant (Contravariant)
import qualified Data.HashMap.Strict as M
import Data.List (intersperse)
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
import UnliftIO.STM (TChan, TVar, atomically, dupTChan, newBroadcastTChanIO, newTChanIO, newTVarIO, readTChan, readTVar, readTVarIO, writeTChan, writeTVar)

import GHC.Generics (Generic)

import qualified Data.Text as T
import Text.Pretty.Simple (pPrintLightBg)

type DeviceState = M.HashMap PropertyAddress Value

data DeviceStore = DeviceStore
  { devices :: M.HashMap DeviceId Device
  , deviceState :: M.HashMap DeviceId DeviceState
  } deriving (Eq, Generic, Show)

instance ToJSON DeviceStore where
  toEncoding = genericToEncoding defaultOptions

type DeltaBatch = (DeviceId, [(PropertyAddress, Value)])


-- hacks while spiking

creds :: (String, String)
creds = ("automation-service-dev", "<whoops not the real key heh>")

uri :: URI
uri = fromJust $ parseURI $ "mqtts://" <> (fst creds) <> ":" <> (snd creds) <> "@mosquitto:8883"


initMQTTClient :: MQTT.MessageCallback -> MQTTConfig -> IO MQTT.MQTTClient
initMQTTClient msgCB (MQTTConfig {..}) = do
  -- hSetBuffering stdout (BlockBuffering Nothing)

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
      -- putStrLn $ "Implement me -- certtypes: " <> show certtypes <> ", mHashSigs: " <> show mHashSigs <> ", DNs: " <> show dns
      pure $ Just cred'


data DeviceMessage
  = DevicesUpdated
  | Quiescent
  deriving (Eq, Show)

-- move this into its own namespace so names don't collide
data FrontendMessage
  = FDevicesUpdated
  | FDeviceStateUpdated DeltaBatch -- maybe holds deltas?
  | FDeviceCommandReceived DeltaBatch
  deriving (Eq, Show)


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
  -> TChan FrontendMessage
  -> MQTT.MessageCallback
mqttClientCallback deviceStore deviceUpdatesChan frontendUpdatesChan =
  MQTT.SimpleCallback $ \_mc topic msg _props -> do
    case topic of
      "zigbee2mqtt/bridge/devices" -> do
        -- putStrLn $ "devices topic:" <> show topic
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
              writeTChan deviceUpdatesChan DevicesUpdated

      "zigbee2mqtt/bridge/groups" -> do
        pure ()
        -- putStrLn $ "groups topic" <> show topic
        -- putStrLn $ "groups msg:" <> show msg
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
            writeTChan frontendUpdatesChan $ FDeviceStateUpdated (deviceId', deltas)
        -- putStrLn $ "Other topic: " <> show topic
        -- putStrLn $ "Other msg: " <> show msg


type PropertyAddress = T.Text

data StateUpdate = StateUpdate
  { deviceId :: DeviceId
  , stateValues :: [ (PropertyAddress, Value) ]
  } deriving (Eq, Show)

-- probably should make this a Device/Capability module lookup function(s)?
kindLens
  :: (Applicative f, Contravariant f)
  => DeviceId
  -> T.Text
  -> LensLike' f (M.HashMap DeviceId Device) Kind
kindLens deviceId k =
  ix deviceId . capabilities . folded . filtered ((== k) . view property) . kind

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

        normalizeState :: Value -> M.HashMap DeviceId Device -> Value
        normalizeState v devices' =
          case v of
            String stateTxt
              | devices' ^? kindLens deviceId "state" . valueOn == Just stateTxt ->
                Bool True
              | devices' ^? kindLens deviceId "state" . valueOff == Just stateTxt ->
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


denormalizeVal DeviceStore{ devices } deviceId propAddr val = 
  case M.lookup deviceId devices of
    Just device ->
      case (propAddr, val) of
        ("state", Bool True) ->
          String $ devices ^. kindLens deviceId "state" . valueOn

        ("state", Bool False) ->
          String $ devices ^. kindLens deviceId "state" . valueOff

        (_, _) ->
          val

    Nothing ->
      val
  

deltasToState :: DeviceStore -> DeltaBatch -> Value
deltasToState deviceStore (deviceId, deltas) = 
  Object $ deltasToState' KM.empty deltas
  where
    deltasToState' :: KM.KeyMap Value -> [(PropertyAddress, Value)] -> KM.KeyMap Value
    deltasToState' obj =
      foldl'
      (\obj' (propAddr, val) ->
         let
           denormalizedVal = denormalizeVal deviceStore deviceId propAddr val
         in
           case T.split (== '.') $ propAddr of
             [onlyAddr] ->
               KM.insert (fromText onlyAddr) denormalizedVal obj'

             (firstAddr:restAddr) ->
               let
                 -- not safe, I know...
                 (Object obj'') = fromMaybe (Object KM.empty) $ KM.lookup (fromText firstAddr) obj'
               in
                 --- maybe should use KM.alterF here?
                 KM.insert
                  (fromText firstAddr)
                  (Object $ deltasToState' obj'' [(fold $ intersperse "." restAddr, denormalizedVal)])
                  obj'

             _ ->
               obj'
      )
      obj

--   
-- 
--               for each delta@(key, value)
--                 if key has a dot
--                   we split it
--                   we check if we already have a map at the address of the first key, if we do we pass it into this function again with the object, otherwise we create a new one and do the same
--                 otherwise if not we first process the value with the device schema to normalize any values, and then directly stuff the value into the map


initZigbee2MQTTAdapter :: MQTTConfig -> IO ()
initZigbee2MQTTAdapter mqttConfig = do
  deviceStore <- newTVarIO $ DeviceStore M.empty M.empty

  deviceUpdatesChan <- newBroadcastTChanIO
  deviceUpdatesChanListener <- atomically $ dupTChan deviceUpdatesChan

  frontendUpdatesChan <- newBroadcastTChanIO
  frontendUpdatesChanListener <- atomically $ dupTChan frontendUpdatesChan

  mc2 <- initMQTTClient (mqttClientCallback deviceStore deviceUpdatesChan frontendUpdatesChan) mqttConfig

  -- kinda don't need to resubscribe to a lot of devices, but it
  -- should be a replacement operation even if a bit inefficient. Can
  -- clean it up later to avoid resubscribing to existing device
  -- topics
  void $ liftIO $ async $
    let
      go = do
        msg <- atomically $ readTChan deviceUpdatesChanListener
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
            atomically $ writeTChan frontendUpdatesChan FDevicesUpdated
            go
          
          _ -> go
    in
      go

  -- frontend (receiving) mock
  void $ liftIO $ async $
    let
      go stage = do
        -- when we start this we deliberately send an initial
        -- DevicesUpdated message to ensure the initial snapshot is sent:
        when (stage == "init") $
          atomically $ writeTChan frontendUpdatesChan FDevicesUpdated
        frontendUpdate <- atomically $ readTChan frontendUpdatesChanListener
        deviceStore' <- readTVarIO deviceStore

        case frontendUpdate of
          FDevicesUpdated -> do
            pPrintLightBg "FDevicesUpdated"
            -- pPrintLightBg $ encodePretty deviceStore'
            -- LBS8.hPutStrLn stdout $ encode deviceStore'

          FDeviceStateUpdated (deviceId, deltas) -> do
            pPrintLightBg "FDeviceStateUpdated"
            -- pPrintLightBg "deltas"
            LBS8.hPutStrLn stdout $ encode (deviceId, deltas)
            -- putStrLn $ encode $ M.fromList [(deviceId, M.fromList deltas)]
            -- pPrintLightBg $ encode deviceId

          -- this represents a command coming _back_ from the frontend
          -- to be processed
          FDeviceCommandReceived (deviceId, deltas) -> do
            pPrintLightBg "FDeviceCommandReceived"
                
            let
              deviceName = (devices deviceStore') ^. ix deviceId . name
              stateUpdate = encode $ deltasToState deviceStore' (deviceId, deltas)

            pPrintLightBg $ deviceName
            pPrintLightBg $ stateUpdate 

            for_ (mkTopic $ "zigbee2mqtt/" <> deviceName <> "/set") $ \topic ->
              MQTT.publish mc2 topic stateUpdate False


        go "run"
    in
      go "init"

  -- running this in a separate thread as its meant to be the frontend
  -- sending commands _back_ to the backend, and we don't want waiting
  -- on the frontendUpdatesChanListener blocking loop above
  void $ liftIO $ async $
    let
      go stage = do
        -- atomically $ writeTChan frontendUpdatesChan FDevicesUpdated

        -- give this a good wait before we actually attempt to send anything
        threadDelay $ 10000 * 1000

        when (stage == "init") $ do
          let
            basementBlackSigneDeltas =
              ("0x001788010c52373e"
              , [ ("state", Bool True)
                , ("color.x", Number 0.123)
                , ("color.y", Number 0.123)
                , ("brightness", Number 125)
                ]
              )
          atomically $ writeTChan frontendUpdatesChan $ FDeviceCommandReceived basementBlackSigneDeltas

        go "run"
    in
      go "init"


  threadDelay $ 1000 * 1000

  for_ ["zigbee2mqtt/bridge/devices", "zigbee2mqtt/bridge/groups"] $ \topicText ->
    for_ (mkFilter topicText) $ \topic -> do
      -- putStrLn $ "subscribing to topic " <> (show topicText)
      MQTT.subscribe mc2 [(topic, MQTT.subOptions)] []

  -- TODO next: test sending commands back, mocking frontend devices,
  -- dig into adapter architecture a bit more
