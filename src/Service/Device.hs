{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}

module Service.Device
  ( DeviceId
  , Device(..)
  , category
  , id
  , manufacturer
  , model
  , name
  , parseTopic
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
  , decode
  , defaultOptions
  , encode
  , fieldLabelModifier
  , genericToEncoding
  , withText
  )
import Data.Maybe (fromJust)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.MQTT.Topic (Topic(..), mkTopic)

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

instance FromJSON Device

instance FromJSON Topic where
  parseJSON = withText "Topic" $ pure . parseTopic

instance ToJSON Topic where
  toJSON t = toJSON (unTopic t)

parseTopic :: Text -> Topic
parseTopic t =
  case mkTopic t of
    Just topic' -> topic'
    Nothing -> fromJust . mkTopic $ "failedToMakeTopic"

toLuaDevice :: Device -> Maybe Value
toLuaDevice = decode . encode
