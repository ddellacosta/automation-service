{-# LANGUAGE TemplateHaskell #-}

module Service.Device
  ( DeviceId
  , Device(..)
  , category
  , id
  , manufacturer
  , model
  , name
  , toLuaDevice
  , topic
  , topicGet
  , topicSet
  )
where

import Prelude hiding (id)

import Control.Lens (makeFieldsNoPrefix)
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value
  , (.:)
  , (.:?)
  , decode
  , defaultOptions
  , encode
  , fieldLabelModifier
  , genericToEncoding
  , withObject
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.MQTT.Topic (Topic(..))
import Service.MQTT.Zigbee2MQTT as Zigbee2MQTT

type DeviceId = Text

data Device = Device
  { _id :: DeviceId
  , _name :: Text
  , _category :: Text
  , _manufacturer :: Maybe Text
  , _model :: Maybe Text
  , _topic :: Topic
  , _topicGet :: Topic
  , _topicSet :: Topic
  }
  deriving (Generic, Show, Eq)

makeFieldsNoPrefix ''Device

instance ToJSON Device where
  toEncoding = genericToEncoding defaultOptions
    { fieldLabelModifier = drop 1
    }

instance FromJSON Device where
  parseJSON = withObject "Device" $ \d -> do
    id' <- d .: "ieee_address"
    name' <- d .: "friendly_name"
    category' <- d .: "type"
    manufacturer' <- d .:? "manufacturer"
    model' <- d .:? "model_id"
    pure $ Device
      id'
      name'
      category'
      manufacturer'
      model'
      (Zigbee2MQTT.mkTopic name')
      (Zigbee2MQTT.mkGetTopic name')
      (Zigbee2MQTT.mkSetTopic name')

toLuaDevice :: Device -> Maybe Value
toLuaDevice = decode . encode
