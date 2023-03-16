{-# LANGUAGE TemplateHaskell #-}

module Service.Group
  ( Group(..)
  , GroupId
  , Member(..)
  , id
  , members
  , name
  , scenes
  , toLuaGroup
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
import Service.Messages.Zigbee2MQTT as Zigbee2MQTT

type GroupId = Text

data Member = Member
  { _endpoint :: Int
  , _ieeeAddress :: Text
  }
  deriving (Show, Generic, Eq)

instance ToJSON Member where
  toEncoding = genericToEncoding defaultOptions
    { fieldLabelModifier = drop 1
    }

instance FromJSON Member where
  parseJSON = withObject "Member" $ \g ->
    Member <$> g .: "ieee_address" <*> g .: "endpoint"

data Group = Group
  { _id :: GroupId
  , _name :: Text
  , _members :: [Member]
  , _scenes :: [Text]
  , _topic :: Topic
  , _topicSet :: Topic
  , _topicGet :: Topic
  }
  deriving (Show, Generic, Eq)

makeFieldsNoPrefix ''Group

instance ToJSON Group where
  toEncoding = genericToEncoding defaultOptions
    { fieldLabelModifier = drop 1
    }

instance FromJSON Group where
  parseJSON = withObject "Group" $ \g -> do
    name' <- g .: "friendly_name"
    id' <- g .: "id"
    members' <- g .: "members"
    -- _scenes <- g .: "scenes"
    pure $ Group
      id'
      name'
      members'
      []
      (Zigbee2MQTT.mkTopic name')
      (Zigbee2MQTT.mkGetTopic name')
      (Zigbee2MQTT.mkSetTopic name')

toLuaGroup :: Group -> Maybe Value
toLuaGroup = decode . encode
