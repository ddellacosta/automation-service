{-# LANGUAGE RecordWildCards #-}

module Service.Adapters.Zigbee2MQTT
 ( initMQTTClient
 , initZigbee2MQTTAdapter
 , mqttClientCallback
 )
where

import Control.Applicative ((*>))
import Control.Monad (when, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Array, FromJSON(..), Object, (.:), (.:?), decode, withArray, withObject)
import Data.Aeson.Types (Parser, Value(..), parseEither, parseFieldMaybe)
import Data.Bits ((.&.))
import Data.Either (Either(..), fromRight)
import Data.Foldable (foldl', for_)
import Data.Hashable (Hashable (..))
import qualified Data.HashMap.Strict as M
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Maybe (Maybe(..), fromMaybe, maybe, fromJust)
import qualified Data.Text as T
import Data.Traversable (for, forAccumM)
import Data.Vector (toList)
import Data.X509.CertificateStore (makeCertificateStore, readCertificateStore)
import GHC.Generics (Generic)
import Network.Connection (TLSSettings (..))
import qualified Network.MQTT.Client as MQTT
import Network.MQTT.Topic (mkFilter)
import Network.TLS (ClientHooks (..), ClientParams (..), Credentials (..), Shared (..), Supported (..), Version (..), credentialLoadX509, defaultParamsClient)
import Network.TLS.Extra.Cipher (ciphersuite_default)
import Network.URI (URI, parseURI)
import Prelude (Bool, Double, Eq, Int, IO, Show, String, (.), ($), (<$>), (>>=), (>), (/=), (<>), (*), flip, fst, putStrLn, pure, show, snd)
-- import Service.App (Logger)
-- import qualified Service.App as App
import Service.Env (LogLevel (..), MQTTConfig (..), Subscriptions)
-- import Service.MQTT.Zigbee2MQTT as Zigbee2MQTT
-- pPrint is supposed to be better for dark background, but doesn't seem like it.
import Text.Pretty.Simple (pPrintLightBg)
import UnliftIO.Async (async)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)

data NumericPreset = NumericPreset
  { pName :: T.Text
  , pValue :: Int
  , pDescription :: T.Text
  } deriving (Eq, Show)

data Kind
  = Binary
    { valueOn :: T.Text
    , valueOff :: T.Text
    , valueToggle :: Maybe T.Text
    }
  | Composite -- will these show up at all?
  | Enum
    { values :: [ T.Text ] }
  | List -- TODO
  | Numeric
    { unit :: Maybe T.Text
    , valueMax :: Maybe Int
    , valueMin :: Maybe Int
    , valueStep :: Maybe Double
    , presets :: Maybe [NumericPreset]
    }
  | Text -- TODO
  deriving (Eq, Show)

-- |
-- | https://www.zigbee2mqtt.io/guide/usage/exposes.html#access
-- |
data Access
  = Reports
  | Writeable
  | Readable -- implies Reports as well for Zigbee2MQTT, but may not for Matter
  deriving (Eq, Generic, Show)

instance Hashable Access

reports, writeable, readable :: Int -> Bool
reports a = 1 .&. a > 0
writeable a = 2 .&. a > 0
readable a = 4 .&. a > 0

toAccess :: Int -> HashSet Access
toAccess access =
  foldl'
  (\accessSet (accessPred, accessVal) ->
     if accessPred access
     then
       HS.insert accessVal accessSet
     else
       accessSet
  )
  HS.empty
  [(reports, Reports), (writeable, Writeable), (readable, Readable)]


data Capability = Capability
  { capName :: T.Text
  , label :: T.Text
  , property :: T.Text
  , description :: Maybe T.Text
  , kind :: Kind
  , access :: HashSet Access
  } deriving (Eq, Show)

--
-- Capability.property
--
type Property = T.Text

type Capabilities = M.HashMap Property Capability

data Device = Device
  { name :: T.Text
  , ieeeAddress :: T.Text
  , manufacturer :: Maybe T.Text
  , modelId :: Maybe T.Text
  , capabilities :: Capabilities
  } deriving (Eq, Show)

--
-- Device.ieeeAddress
--
type DeviceId = T.Text

-- this is just a convenience type to allow me to easily call `decode`
-- on the message I get back from zigbee2mqtt/bridge/devices initially
data Devices = Devices
  { loadDevices :: [Device]
  } deriving (Eq, Show)

data DeviceStore = DeviceStore
  { devices :: M.HashMap DeviceId Device
  } deriving (Eq, Show)

-- probably need to handle an exception here as this isn't exhaustive,
-- or make a null type or something or pick a default, but don't like
-- any of those options really
parseKind :: Object -> T.Text -> Parser Kind
parseKind capObj = \case
 "binary" -> do
   valueOn <- capObj .: "value_on"
   valueOff <- capObj .: "value_off"
   valueToggle <- capObj .:? "value_toggle"
   pure $ Binary valueOn valueOff valueToggle
 "composite" -> pure Composite
 "enum" -> Enum <$> capObj .: "values"
 "list" -> pure List
 "numeric" -> do
   unit <- capObj .:? "unit"
   valueMax <- capObj .:? "value_max"
   valueMin <- capObj .:? "value_min"
   valueStep <- capObj .:? "value_step"
   -- presets <- capObj .:? "presets"
   pure $ Numeric unit valueMax valueMin valueStep Nothing
 "text" -> pure Text

instance FromJSON Capability where
  parseJSON = withObject "Capability" $ \c -> do
    capName <- c .: "name"
    label <- c .: "label"
    property <- c .: "property"
    description <- c .:? "description"
    kind <- c .: "type"
    kindParsed <- parseKind c kind
    access <- c .: "access"
    pure $
      Capability
        capName
        label
        property
        description
        kindParsed
        (toAccess access)

attachCapabilityKind :: Value -> Capability -> Parser [Capability]
attachCapabilityKind capObj capability =
  case capObj of 
    (Object c) -> do
      mFeatures :: Maybe Array <- c .:? "features"

      case mFeatures of
        Just features -> for (toList features) $ \c' -> do
          childCap <- parseJSON c'
          pure $ Capability
            ((capName capability) <> "-" <> (capName childCap))
            ((label capability) <> "-" <> (label childCap))
            ((property capability) <> "." <> (property childCap))
            (description capability) 
            (kind childCap)
            (access childCap)

        Nothing -> pure $ [ capability ]

    _ -> pure [capability]

parseCapabilities :: Maybe Array -> Parser Capabilities
parseCapabilities mExposes = do
  let
    capabilities =
      case mExposes of
        Just exposes -> flip parseEither exposes $ \cs ->
          forAccumM M.empty (toList cs) $ \acc c' -> do
            let
              (cap :: Either String Capability) = parseEither parseJSON c'
            case cap of
              Right cap' -> do
                caps <- attachCapabilityKind c' cap'
                pure (foldl' (\acc' c -> M.insert (property c) c acc') acc caps, ())
              Left _errorMsg -> do
                mFeatures <- withObject "features" (\features -> features .:? "features") c'
                caps <- parseCapabilities mFeatures
                pure (caps, ())
        Nothing -> Right (M.empty, [()])
  pure $ case capabilities of
    Right (capabilities', _unit) -> capabilities'
    Left _errorMsg -> M.empty

instance FromJSON Device where
  parseJSON = withObject "Device" $ \d -> do
    name <- d .: "friendly_name"
    ieeeAddress <- d .: "ieee_address"
    manufacturer <- d .:? "manufacturer"
    modelId <- d .:? "model_id"
    mExposes <-
      d .:? "definition" >>= maybe (pure Nothing) (flip parseFieldMaybe "exposes")
    capabilities <- parseCapabilities mExposes
    pure $ Device name ieeeAddress manufacturer modelId capabilities

instance FromJSON Devices where
 parseJSON = withArray "Devices" $ \a ->
    Devices <$> for (toList a) parseJSON


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


-- | Returns a SimpleCallback which is an alias for type
-- MQTTClient -> Topic -> ByteString -> [Property] -> IO ()
--
mqttClientCallback
  :: TVar DeviceStore
  -> MQTT.MessageCallback
--  :: (Logger logger)
--  => LogLevel
--  -> logger
--  -> TVar Subscriptions
--  -> MQTT.MessageCallback
-- mqttClientCallback logLevelSet logger subscriptions =
mqttClientCallback deviceStore =
  MQTT.SimpleCallback $ \_mc topic msg _props -> do
    case topic of
      "zigbee2mqtt/bridge/devices" -> do
        putStrLn $ "devices topic:" <> show topic
        -- putStrLn $ "devices msg:" <> show msg
        -- putStrLn $ "devices parsed msg:" <> show (decode msg :: Maybe Devices)
        for_ (decode msg :: Maybe Devices) $ \devices ->
          let 
            deviceMap = M.fromList $ (\device -> ((ieeeAddress device), device)) <$> (loadDevices devices)
          in
            atomically . (writeTVar deviceStore) $ DeviceStore deviceMap
      "zigbee2mqtt/bridge/groups" -> do
        putStrLn $ "groups topic:" <> show topic
        putStrLn $ "groups msg:" <> show msg
      _ -> do
        putStrLn $ "Other topic: " <> show topic
        putStrLn $ "Other msg: " <> show msg



-- needs to load up devices and groups from MQTT on start
  -- parse into structures
-- then needs to go through each item in both collections and subscribe to updates for all

initZigbee2MQTTAdapter :: MQTTConfig -> IO ()
initZigbee2MQTTAdapter mqttConfig = do
  deviceStore <- newTVarIO $ DeviceStore M.empty

  -- needs to be started before subs or else may miss initial responses!
  void $ liftIO $ async $
    let
      go = do
        devices <- readTVarIO deviceStore
        threadDelay $ 500 * 1000
        devicesUpdated <- readTVarIO deviceStore
        _ <- when (devices /= devicesUpdated)
          (putStrLn "Devices were updated: ") *> (liftIO $ pPrintLightBg devicesUpdated) *> go
        go
    in
      go

  threadDelay $ 1000 * 1000

  mc2 <- initMQTTClient (mqttClientCallback deviceStore) mqttConfig

  for_ ["zigbee2mqtt/bridge/devices", "zigbee2mqtt/bridge/groups"] $ \topicText ->
    for_ (mkFilter topicText) $ \topic -> do
      putStrLn $ "subscribing to topic " <> (show topicText)
      MQTT.subscribe mc2 [(topic, MQTT.subOptions)] []

  -- TODO next: sub to individual device topics from what I've loaded
  -- into schema, and then start handling device state including
  -- generating deltas, then can finally test the last step of sending
  -- commands back, mocking frontend devices
