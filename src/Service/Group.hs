{-# LANGUAGE TemplateHaskell #-}

module Service.Group
  ( Group(..)
  , GroupId
  , Member(..)
  , Scene(..)
  , id
  , endpoint
  , memberId
  , members
  , name
  , scenes
  , sceneId
  , sceneName
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

type GroupId = Int

data Member = Member
  { _memberId :: Text
  , _endpoint :: Int
  }
  deriving (Show, Generic, Eq)

makeFieldsNoPrefix ''Member

instance ToJSON Member where
  toEncoding = genericToEncoding defaultOptions
    { fieldLabelModifier = drop 1
    }

instance FromJSON Member where
  parseJSON = withObject "Member" $ \g ->
    Member <$> g .: "ieee_address" <*> g .: "endpoint"

data Scene = Scene
  { _sceneId :: Int
  , _sceneName :: Text
  }
  deriving (Show, Generic, Eq)

makeFieldsNoPrefix ''Scene

instance ToJSON Scene where
  toEncoding = genericToEncoding defaultOptions
    { fieldLabelModifier = drop 1
    }

instance FromJSON Scene where
  parseJSON = withObject "Scene" $ \g ->
    Scene <$> g .: "id" <*> g .: "name"

data Group = Group
  { _id :: GroupId
  , _name :: Text
  , _members :: [Member]
  , _scenes :: [Scene]
  , _topic :: Topic
  , _topicGet :: Topic
  , _topicSet :: Topic
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
    scenes' <- g .: "scenes"
    pure $ Group
      id'
      name'
      members'
      scenes'
      (Zigbee2MQTT.mkTopic name')
      (Zigbee2MQTT.mkGetTopic name')
      (Zigbee2MQTT.mkSetTopic name')

toLuaGroup :: Group -> Maybe Value
toLuaGroup = decode . encode
