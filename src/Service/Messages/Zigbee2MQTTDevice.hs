{-# LANGUAGE DeriveAnyClass #-}

module Service.Messages.Zigbee2MQTTDevice
  ( Message(..)
  , parseDevices
  , topic
  )
  where

import Prelude hiding (id)

import Control.Lens ((^?))
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON, ToJSON, Value, decode)
import Data.Aeson.Lens (key)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Service.Device (Device(..), parseTopic)
import Network.MQTT.Topic (Topic(..))

data Message where
  BridgeDevices :: [Value] -> Message
  deriving (Show, Generic, ToJSON, FromJSON)

--
-- TODO this should return something better than Maybe to let us know
-- what happened when this fails, if possible
--
parseDevices :: ByteString -> Maybe [Device]
parseDevices devicesRawJSON =
  catMaybes <$> (fmap parseDevice) <$> (decode devicesRawJSON :: Maybe [Value])

  where
    parseDevice d = case (deviceFields d) of
      -- I don't know what's up with this formatting either, but it
      -- won't parse without it unless it's all on one line, and I'd
      -- rather have this
      [   Just (Aeson.String id)
        , Just (Aeson.String name)
        , Just (Aeson.String category)
        , mManufacturer
        , mModel
        ] ->
        Just $
          Device
            id
            name
            category
            (toText <$> mManufacturer)
            (toText <$> mModel)
            (parseTopic . toTopicString $ name)
            (parseTopic . toGetTopicString $ name)
            (parseTopic . toSetTopicString $ name)

      _ -> Nothing

    deviceFields d =
      [ d ^? key "ieee_address"
      , d ^? key "friendly_name"
      , d ^? key "type"
      , d ^? key "manufacturer"
      , d ^? key "model_id"
      ]

    toText (Aeson.String s) = s
    toText _ = ""

    toTopicString friendlyName = "zigbee2mqtt/" <> friendlyName
    toGetTopicString = (<> "/get") . toTopicString
    toSetTopicString = (<> "/set") . toTopicString

-- https://www.zigbee2mqtt.io/guide/usage/mqtt_topics_and_messages.html#zigbee2mqtt-bridge-devices
topic :: Topic
topic = "zigbee2mqtt/bridge/devices"
