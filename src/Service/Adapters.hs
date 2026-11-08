module Service.Adapters
 ( initAdapters
 )
where

import Text.Pretty.Simple (pPrintLightBg)

import Control.Lens ((^.), (^?), ix)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(..), Value(..), encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Foldable (for_)
import qualified Network.MQTT.Client as MQTT
import Network.MQTT.Client (MQTTClient)
import Network.MQTT.Topic (mkFilter, mkTopic)
import Prelude (Bool(..), Double, Int, IO, String, (.), ($), (<$>), (<>), (*), (==), (/), fromIntegral, otherwise)
import Service.Adapters.Device (name)
import Service.Adapters.DeviceStore (DeviceStore(..), DeviceMessage(..), FrontendMessage(..))
import Service.Adapters.Zigbee2MQTT as Z2MAdapter
import System.IO (stdout)
import System.Random (randomRIO)
import UnliftIO.Async (async)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM (TChan, TVar, atomically, readTChan, readTVarIO, writeTChan)

initAdapters
  :: MQTTClient
  -> TVar DeviceStore
  -> TChan DeviceMessage
  -> TChan DeviceMessage
  -> TChan FrontendMessage
  -> TChan FrontendMessage
  -> IO ()
initAdapters mc deviceStore _deviceUpdatesChan deviceUpdatesChanListener frontendUpdatesChan frontendUpdatesChanListener = do

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
                MQTT.subscribe mc [(topic, MQTT.subOptions)] []
              for_ (mkTopic getTopicText) $ \topic -> do
                MQTT.publish mc topic "{\"state\": \"\"}" False
            atomically $ writeTChan frontendUpdatesChan FDevicesUpdated
            go
          
          ESPHomeDeviceUpdated topic -> do
            pPrintLightBg ("ESPHomeDeviceUpdated" :: String)
            pPrintLightBg topic
            go

          GroupsUpdated -> do
            atomically $ writeTChan frontendUpdatesChan FGroupsUpdated
            go
    in
      go

  -- frontend (receiving) mock
  void $ liftIO $ async $
    let
      go :: String -> IO ()
      go stage = do
        -- when we start this we deliberately send an initial
        -- DevicesUpdated message to ensure the initial snapshot is sent:
        when (stage == "init") $
          atomically $ writeTChan frontendUpdatesChan FDevicesUpdated
        frontendUpdate <- atomically $ readTChan frontendUpdatesChanListener
        deviceStore' <- readTVarIO deviceStore

        case frontendUpdate of
          FDevicesUpdated -> do
            pPrintLightBg ("FDevicesUpdated" :: String)
            -- pPrintLightBg $ encodePretty deviceStore'
            -- LBS8.hPutStrLn stdout $ encode deviceStore'

          FGroupsUpdated -> do
            pPrintLightBg ("FGroupsUpdated" :: String)
            pPrintLightBg $ encodePretty (groups deviceStore')

          FDeviceStateUpdated (deviceId, deltas) -> do
            pPrintLightBg ("FDeviceStateUpdated" :: String)
            -- pPrintLightBg "deltas"
            LBS8.hPutStrLn stdout $ encode (deviceId, deltas)
            -- putStrLn $ encode $ M.fromList [(deviceId, M.fromList deltas)]
            -- pPrintLightBg $ encode deviceId

          -- this represents a command coming _back_ from the frontend
          -- to be processed
          FDeviceCommandReceived (deviceId, deltas) -> do
            pPrintLightBg ("FDeviceCommandReceived" :: String)
                
            let
              mDeviceName = (devices deviceStore') ^? ix deviceId . name
              stateUpdate = encode $ Z2MAdapter.deltasToState deviceStore' (deviceId, deltas)

            pPrintLightBg $ mDeviceName
            pPrintLightBg $ stateUpdate 

            for_ mDeviceName $ \deviceName ->
              for_ (mkTopic $ "zigbee2mqtt/" <> deviceName <> "/set") $ \topic ->
                MQTT.publish mc topic stateUpdate False

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
            | otherwise =
              ("0x001788010c52373e"
              , [ ("state", Bool True)
                , ("color.x", toJSON colorX)
                , ("color.y", toJSON colorY)
                , ("brightness", Number 125)
                ]
              )

        -- messing around to prove this sets too
        -- atomically $
        --   writeTChan frontendUpdatesChan $
        --     FDeviceCommandReceived basementBlackSigneDeltas

        if which == 0 then
          go 1
        else
          go 0
    in
      go 1


  threadDelay $ 1000 * 1000

  for_ ["zigbee2mqtt/bridge/devices", "zigbee2mqtt/bridge/groups", "esphome/#"] $ \topicText ->
    for_ (mkFilter topicText) $ \topic -> do
      -- putStrLn $ "subscribing to topic " <> (show topicText)
      MQTT.subscribe mc [(topic, MQTT.subOptions)] []
