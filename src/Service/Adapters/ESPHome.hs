{-# LANGUAGE RecordWildCards #-}

module Service.Adapters.ESPHome
 ( topicCallbacks
 )
where

import Text.Pretty.Simple (pPrintLightBg)

import Prelude (IO, ($), pure)
import Data.Maybe (Maybe(..))
import qualified Data.Text as T
import Network.MQTT.Topic (Topic, unTopic)
import Service.Adapters.MQTT (Captures(..), TopicCallbacks(..), TopicCallbackArgs(..))
import Service.Adapters.DeviceStore (DeviceMessage(..))
import UnliftIO.STM (atomically, writeTChan)


matchesESPHome :: TopicCallbackArgs -> Topic -> IO (Maybe Captures)
matchesESPHome _callbackArgs msgTopic = do
  case T.splitOn "/" $ unTopic msgTopic of
    "esphome":_rest -> do
      pure (Just $ ESPHomeCaptures msgTopic)

    _ ->
      pure Nothing

topicCallbacks :: TopicCallbacks
topicCallbacks
  = TopicCallbacks [(matchesESPHome, espHomeHandler)]

espHomeHandler :: TopicCallbackArgs -> Captures -> IO ()
espHomeHandler (TopicCallbackArgs { topic, deviceUpdatesChan }) captures = do
  atomically $
    writeTChan deviceUpdatesChan $
      ESPHomeDeviceUpdated topic
