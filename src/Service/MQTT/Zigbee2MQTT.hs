module Service.MQTT.Zigbee2MQTT
  ( devicesTopic
  , groupsTopic
  , mkGetTopic
  , mkSetTopic
  , mkTopic
  , mkTopicString
  )
  where

import Data.Text (Text)
import Network.MQTT.Topic (Topic (..))
import Service.MQTT.Topic (parseTopic)

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
