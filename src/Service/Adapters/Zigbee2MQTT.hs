{-# LANGUAGE RecordWildCards #-}

module Service.Adapters.Zigbee2MQTT
 ( deltasToState
 , topicCallbacks
 )
where

-- import Text.Pretty.Simple (pPrintLightBg)

import Control.Applicative ((*>))
import Control.Lens ((^?), (^..), _Just, folded, ix)
import Control.Monad (guard, when)
import Data.Aeson (Value(..), (.:), decode, withObject)
import Data.Aeson.Key (fromText, toText)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.KeyMap (foldMapWithKey)
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (foldl', for_)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as HashSet
import Data.Maybe (Maybe(..), fromMaybe)
import qualified Data.Text as T
import Network.MQTT.Topic (Topic, unTopic)
import Prelude (Bool(..), Eq, IO, Show, (.), ($), (=<<), (<>), (>), (==), (/=), (&&), filter, not, null, pure)
import Service.Adapters.Capability (Address, _Binary, kind)
import Service.Adapters.Device (Device(..), DeviceId, Devices(..), capabilities, name)
import Service.Adapters.DeviceStore (DeltaBatch, DeviceState, DeviceStore(..), DeviceMessage(..), FrontendMessage(..))
import Service.Adapters.Group (Groups(..))
import Service.Adapters.MQTT (Captures(..), TopicCallbacks(..), TopicCallbackArgs(..))
import UnliftIO.STM (atomically, readTVar, readTVarIO, writeTChan, writeTVar)

matches :: Topic -> TopicCallbackArgs -> Topic -> IO (Maybe Captures)
matches matchTopic _callbackArgs msgTopic =
  pure $
    guard (matchTopic == msgTopic) *> pure (Zigbee2MQTTCaptures matchTopic)

matchesDeviceStateTopic :: TopicCallbackArgs -> Topic -> IO (Maybe Captures)
matchesDeviceStateTopic TopicCallbackArgs { deviceStore } msgTopic = do
  deviceStore' <- readTVarIO deviceStore

  let
    deviceNames = HashSet.fromList $ devices deviceStore' ^.. folded . name
  
  pure $
    case T.splitOn "/" $ unTopic msgTopic of
      ["zigbee2mqtt", deviceName] -> do
        guard (HashSet.member deviceName deviceNames) *>
          pure (Zigbee2MQTTCaptures msgTopic)

      _ ->
        Nothing

topicCallbacks :: TopicCallbacks
topicCallbacks = TopicCallbacks 
  [ (matches "zigbee2mqtt/bridge/devices", deviceSpecsCallback)
  , (matches "zigbee2mqtt/bridge/groups", groupSpecsCallback)
  , (matchesDeviceStateTopic, deviceStateCallback)
  ]

deviceSpecsCallback :: TopicCallbackArgs -> Captures -> IO ()
deviceSpecsCallback (TopicCallbackArgs { msg, deviceStore, deviceUpdatesChan }) (Zigbee2MQTTCaptures _topic) =
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
deviceSpecsCallback _topicCallbackArgs _captures = pure ()

groupSpecsCallback :: TopicCallbackArgs -> Captures -> IO ()
groupSpecsCallback (TopicCallbackArgs { msg, deviceStore, deviceUpdatesChan }) (Zigbee2MQTTCaptures _topic) =
  -- putStrLn $ show msg
  for_ (decode msg :: Maybe Groups) $ \groups ->
    atomically $ do
      deviceStore' <- readTVar deviceStore
      -- ditto question from Devices above wrt cleanup
      writeTVar deviceStore $
        DeviceStore (devices deviceStore') (deviceState deviceStore') (loadGroups groups)
      writeTChan deviceUpdatesChan GroupsUpdated
groupSpecsCallback _topicCallbackArgs _captures = pure ()

--
-- what to do about this...
--

deviceStateCallback :: TopicCallbackArgs -> Captures -> IO ()
deviceStateCallback (TopicCallbackArgs { msg, deviceStore, frontendUpdatesChan }) (Zigbee2MQTTCaptures _topic) =
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
deviceStateCallback _topicCallbackArgs _capture = pure ()  


calculateDeltas
  :: DeviceState -- M.HashMap Address Value
  -> [ (Address, Value) ]
  -> [ (Address, Value) ]
calculateDeltas currentState =
  filter (\(prop, val) -> M.lookup prop currentState /= Just val)


data StateUpdate = StateUpdate
  { deviceId :: DeviceId
  , stateValues :: [ (Address, Value) ]
  } deriving (Eq, Show)


normalizeState :: T.Text -> Value -> Maybe Device -> Value
normalizeState k v mDevice =
  case (mDevice ^? _Just . capabilities . ix k . kind . _Binary, v) of
    (Just (valueOn', valueOff', _toggle), String txt)
      | txt == valueOn' -> Bool True
      | txt == valueOff' -> Bool False

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
              let
                pathKey = prepend prefix $ toText k
              in
                [(pathKey, (normalizeState pathKey v mDevice))]

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
  foldl'
    (\o (propAddr, val) ->
       deltasToState' o (propAddr, denormalizeState propAddr val mDevice)
    )
    (Object KM.empty)
    deltas
  where
    mDevice = devices ^? ix deviceId

    deltasToState' :: Value -> (Address, Value) -> Value
    deltasToState' (Object obj) (propAddr, val) =
      case T.breakOn "." propAddr of
        (onlyAddr, "") ->
          Object $ KM.insert (fromText onlyAddr) val obj

        (firstAddr, restAddr) ->
          let
            childObj =
              fromMaybe (Object KM.empty) $ KM.lookup (fromText firstAddr) obj
          in 
            --- maybe should use KM.alterF here?
            Object $ KM.insert
              (fromText firstAddr)
              (deltasToState' childObj (T.drop 1 restAddr, val))
              obj
    -- we probably shouldn't get here but this discards any nested
    -- value, kinda not so great
    deltasToState' val' (_propAddr, _val) = val'
