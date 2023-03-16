{-# LANGUAGE DeriveAnyClass #-}

module Service.Messages.Zigbee2MQTT
  ( devicesTopic
  , groupsTopic
  , mkGetTopic
  , mkSetTopic
  , mkTopic
  , mkTopicString
  )
  where

import Debug.Pretty.Simple (pTraceShow)

import Prelude hiding (id)

import Control.Lens ((^?))
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON, ToJSON, Value, decode)
import Data.Aeson.Lens (key)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Service.Topic (parseTopic)
import Network.MQTT.Topic (Topic(..))

-- https://www.zigbee2mqtt.io/guide/usage/mqtt_topics_and_messages.html#zigbee2mqtt-bridge-devices
devicesTopic :: Topic
devicesTopic = "zigbee2mqtt/bridge/devices"

-- https://www.zigbee2mqtt.io/guide/usage/mqtt_topics_and_messages.html#zigbee2mqtt-bridge-groups 
groupsTopic :: Topic
groupsTopic = "zigbee2mqtt/bridge/groups"

mkTopicString :: Text -> Text
mkTopicString = ("zigbee2mqtt/" <>)

mkTopic :: Text -> Topic
mkTopic = parseTopic . mkTopicString

mkGetTopic :: Text -> Topic
mkGetTopic = parseTopic . (<> "/get") . mkTopicString

mkSetTopic :: Text -> Topic
mkSetTopic = parseTopic . (<> "/set") . mkTopicString
