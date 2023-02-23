{-# LANGUAGE DeriveAnyClass #-}

module Service.Messages.Zigbee2MQTTDevice
  ( Message(..)
  , deviceGetterTopic
  , deviceSetterTopic
  , parseDevices
  )
  where

import Control.Lens ((^..), folded, folding, toListOf)
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON, ToJSON, Value, decode)
import Data.Aeson.Lens (key)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Service.Device (Device(..))
import Network.MQTT.Topic (Topic, mkTopic)

data Message where
  BridgeDevices :: [Value] -> Message
  deriving (Show, Generic, ToJSON, FromJSON)

--
-- TODO this should return something better than Maybe to let us know
-- what happened when this fails
--
parseDevices :: ByteString -> Maybe [Device]
parseDevices devicesRawJSON = parseDevices' <$> (decode devicesRawJSON :: Maybe [Value])
  where
    parseDevices' = toListOf $
        folded
      . folding
        (\d ->
           [d ^.. (key "type" <> key "friendly_name" <> key "ieee_address")])
      . folding
        (\[Aeson.String category, Aeson.String name, Aeson.String id] ->
            [Device id name category])

deviceTopicText :: Device -> Text
deviceTopicText device = "zigbee2mqtt/" <> (_name device)

deviceSetterTopic :: Device -> Maybe Topic
deviceSetterTopic device =
  mkTopic $ deviceTopicText device <> "/set"

deviceGetterTopic :: Device -> Maybe Topic
deviceGetterTopic device =
  mkTopic $ deviceTopicText device <> "/get"
