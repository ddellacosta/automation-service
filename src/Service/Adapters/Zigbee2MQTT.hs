{-# LANGUAGE RecordWildCards #-}

module Service.Adapters.Zigbee2MQTT
 ( initMQTTClient
 , initZigbee2MQTTAdapter
 , mqttClientCallback
 )
where

import Control.Applicative (Applicative)
import Control.Lens (LensLike', (^.), (^..), (^?), _Just, filtered, folded, has, ix, view)
import Control.Monad (void)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(..), Value(..), (.:), decode, defaultOptions, encode, genericToEncoding, withObject)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Key (fromText, toText)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.KeyMap (foldMapWithKey)
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.ByteString.Lazy (ByteString)
import Data.Either (Either(..), fromRight)
import Data.Foldable (foldl', for_)
import Data.Functor.Contravariant (Contravariant)
import qualified Data.HashMap.Strict as M
import Data.Maybe (Maybe(..), fromMaybe, maybe, fromJust)
import qualified Data.Text as T
import Data.X509.CertificateStore (makeCertificateStore, readCertificateStore)
import GHC.Generics (Generic)
import Network.Connection (TLSSettings (..))
import qualified Network.MQTT.Client as MQTT
import Network.MQTT.Topic (mkFilter, mkTopic)
import Network.TLS (ClientHooks (..), ClientParams (..), Credentials (..), Shared (..), Supported (..), Version (..), credentialLoadX509, defaultParamsClient)
import Network.TLS.Extra.Cipher (ciphersuite_default)
import Network.URI (URI, parseURI)
import Prelude (Bool(..), Double, Eq, Int, IO, Show, String, (.), ($), (<$>), (=<<), (<>), (*), (>), (==), (/=), (&&), (/), filter, fromIntegral, fst, not, null, otherwise, putStrLn, pure, show, snd)
import qualified Service.Adapters.Capability as Cap
import Service.Adapters.Capability (Capability, Kind, _Binary, kind, property, valueOff, valueOn)
import Service.Adapters.Device (Device(..), DeviceId, Devices(..), capabilities, name)
import Service.Adapters.Group (Group(..), GroupId, Groups(..))
import Service.Env (MQTTConfig (..))
import System.IO (stdout)
import System.Random (randomRIO)
import Text.Pretty.Simple (pPrintLightBg)
import UnliftIO.Async (async)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM (TChan, TVar, atomically, dupTChan, newBroadcastTChanIO, newTVarIO, readTChan, readTVar, readTVarIO, writeTChan, writeTVar)

type DeviceState = M.HashMap PropertyAddress Value

data DeviceStore = DeviceStore
  { devices :: M.HashMap DeviceId Device
  , deviceState :: M.HashMap DeviceId DeviceState
  , groups :: M.HashMap GroupId Group
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
    clientCertificate cred' (certtypes, mHashSigs, dns) = do
      -- putStrLn $ "Implement me -- certtypes: " <> show certtypes <> ", mHashSigs: " <> show mHashSigs <> ", DNs: " <> show dns
      pure $ Just cred'


data DeviceMessage
  = DevicesUpdated
  | GroupsUpdated
  deriving (Eq, Show)

-- move this into its own namespace so names don't collide
data FrontendMessage
  = FDevicesUpdated
  | FGroupsUpdated
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
        for_ (decode msg :: Maybe Devices) $ \devices ->
          atomically $ do
            deviceStore' <- readTVar deviceStore
            -- should we do some cleanup of the deviceStore here
            -- based on new state schema we pull? Remove
            -- non-existent devices/capabilities/now-disabled
            -- devices, etc.?
            writeTVar deviceStore $
              DeviceStore (loadDevices devices) (deviceState deviceStore') (groups deviceStore')
            writeTChan deviceUpdatesChan DevicesUpdated

      "zigbee2mqtt/bridge/groups" -> do
        -- putStrLn $ show msg
        for_ (decode msg :: Maybe Groups) $ \groups ->
          atomically $ do
            deviceStore' <- readTVar deviceStore
            -- ditto question from Devices above wrt cleanup
            writeTVar deviceStore $
              DeviceStore (devices deviceStore') (deviceState deviceStore') (loadGroups groups)
            writeTChan deviceUpdatesChan GroupsUpdated

      _ ->
        -- this needs to filter based on whether or not this is a
        -- legit device state update message
        atomically $ do
          deviceStore' <- readTVar deviceStore
          for_ (decodeStateUpdate (devices deviceStore') msg) $ \stateUpdate -> do
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
                  M.insertWith
                   M.union
                   (deviceId stateUpdate)
                   (M.fromList $ stateValues stateUpdate)
                   (deviceState deviceStore')
                }

            when (not . null $ deltas) $
              writeTChan frontendUpdatesChan $
                FDeviceStateUpdated (deviceId stateUpdate, deltas)


type PropertyAddress = T.Text

data StateUpdate = StateUpdate
  { deviceId :: DeviceId
  , stateValues :: [ (PropertyAddress, Value) ]
  } deriving (Eq, Show)


normalizeState :: T.Text -> Value -> Maybe Device -> Value
normalizeState k v mDevice =
  case v of
    String txt ->
      case mDevice ^? _Just . capabilities . ix k . kind . _Binary of
        Just (valueOn', valueOff', _toggle)
          | txt == valueOn' -> Bool True
          | txt == valueOff' -> Bool False

        Nothing ->
          v

    Bool stateBool ->
      Bool stateBool

    _ ->
      v

decodeStateUpdate :: M.HashMap DeviceId Device -> ByteString -> Maybe StateUpdate
decodeStateUpdate devices stateUpdateStr =
  parseMaybe parseStateUpdate =<< decode stateUpdateStr
  where
    parseStateUpdate = withObject "StateUpdate" $ \su -> do
      device <- su .: "device"
      deviceId <- device .: "ieeeAddr"

      let
        mDevice = devices ^? ix deviceId

        prepend :: T.Text -> T.Text -> T.Text
        prepend prefix key =
          if T.length prefix > 0
          then
            prefix <> "." <> key
          else
            key

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
              [(prepend prefix $ toText k, (normalizeState (toText k) v mDevice))]

      pure $ StateUpdate deviceId $ stateValues "" su

denormalizeState :: T.Text -> Value -> Maybe Device -> Value
denormalizeState k v mDevice =
  case mDevice ^? _Just . capabilities . ix k . kind . _Binary of
    Just (valueOn', valueOff', _toggle) ->
      case v of
        Bool True -> String valueOn'
        Bool False -> String valueOff'
        _ -> v

    Nothing ->
      v

deltasToState :: DeviceStore -> DeltaBatch -> Value
deltasToState DeviceStore{ devices } (deviceId, deltas) = 
  foldl' deltasToState' (Object KM.empty) deltas
  where
    mDevice = devices ^? ix deviceId

    deltasToState' :: Value -> (PropertyAddress, Value) -> Value
    deltasToState' (Object obj) (propAddr, val) =
      let
        denormalizedVal = denormalizeState propAddr val mDevice 
      in
        case T.breakOn "." propAddr of
          (onlyAddr, "") ->
            Object $ KM.insert (fromText onlyAddr) denormalizedVal obj

          (firstAddr, restAddr) ->
            let
              childObj =
                fromMaybe (Object KM.empty) $ KM.lookup (fromText firstAddr) obj
            in 
              --- maybe should use KM.alterF here?
              Object $ KM.insert
                (fromText firstAddr)
                (deltasToState' childObj (T.drop 1 restAddr, denormalizedVal))
                obj
    -- we probably shouldn't get here but this discards any nested
    -- value, kinda not so great
    deltasToState' val' (_propAddr, _val) = val'

initZigbee2MQTTAdapter :: MQTTConfig -> IO ()
initZigbee2MQTTAdapter mqttConfig = do
  deviceStore <- newTVarIO $ DeviceStore M.empty M.empty M.empty

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
          
          GroupsUpdated -> do
            atomically $ writeTChan frontendUpdatesChan FGroupsUpdated
            go
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

          FGroupsUpdated -> do
            pPrintLightBg "FGroupsUpdated"
            pPrintLightBg $ encodePretty (groups deviceStore')

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
              mDeviceName = (devices deviceStore') ^? ix deviceId . name
              stateUpdate = encode $ deltasToState deviceStore' (deviceId, deltas)

            pPrintLightBg $ mDeviceName
            pPrintLightBg $ stateUpdate 

            for_ mDeviceName $ \deviceName ->
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
      go :: Int -> IO ()
      go which = do
        -- give this a good wait before we actually attempt to send
        -- anything so everything is ready to go
        threadDelay $ 10000 * 1000

        colorXQnt <- randomRIO (0, 800) :: IO Int
        colorYQnt <- randomRIO (0, 900) :: IO Int

        let
          colorX = fromIntegral colorXQnt / 1000 :: Double
          colorY = fromIntegral colorYQnt / 1000 :: Double
          basementBlackSigneDeltas
            | which == 0 =
              ("0x001788010c52373e"
              , [ ("state", Bool False) ]
              )
            | which == 1 =
              ("0x001788010c52373e"
              , [ ("state", Bool True)
                , ("color.x", toJSON colorX)
                , ("color.y", toJSON colorY)
                , ("brightness", Number 125)
                ]
              )

        atomically $
          writeTChan frontendUpdatesChan $
            FDeviceCommandReceived basementBlackSigneDeltas

        if which == 0 then
          go 1
        else
          go 0
    in
      go 1


  threadDelay $ 1000 * 1000

  for_ ["zigbee2mqtt/bridge/devices", "zigbee2mqtt/bridge/groups"] $ \topicText ->
    for_ (mkFilter topicText) $ \topic -> do
      -- putStrLn $ "subscribing to topic " <> (show topicText)
      MQTT.subscribe mc2 [(topic, MQTT.subOptions)] []
