{-# LANGUAGE DeriveAnyClass #-}

module Service.Messages.Zigbee2MQTTDevice
  ( Message(..)
  , deviceGetterTopic
  , deviceSetterTopic
  , parseDevices
  , topic
  )
  where

import Control.Lens ((^..), (^?), folded, folding, toListOf)
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON, ToJSON, Value, decode)
import Data.Aeson.Lens (key)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)
import Service.Device (Device(..))
import Network.MQTT.Topic (Topic, mkTopic)

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
          Device id name category (toText <$> mManufacturer) (toText <$> mModel)

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

deviceTopicText :: Device -> Text
deviceTopicText device = "zigbee2mqtt/" <> (_name device)

deviceSetterTopic :: Device -> Maybe Topic
deviceSetterTopic device =
  mkTopic $ deviceTopicText device <> "/set"

deviceGetterTopic :: Device -> Maybe Topic
deviceGetterTopic device =
  mkTopic $ deviceTopicText device <> "/get"

topic :: Topic
topic = "zigbee2mqtt/bridge/devices"
