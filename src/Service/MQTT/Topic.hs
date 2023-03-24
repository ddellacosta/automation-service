{-# OPTIONS_GHC -fno-warn-orphans #-}

module Service.MQTT.Topic
  ( parseTopic
  )
  where

import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , withText
  )
import Data.Hashable (Hashable(..))
import Data.Maybe (fromJust)
import Data.Text (Text)
import Network.MQTT.Topic (Topic(..), mkTopic, unTopic)

-- to allow for use with Data.HashMap.Strict
instance Hashable Topic where
  hashWithSalt salt = hashWithSalt salt . unTopic

instance FromJSON Topic where
  parseJSON = withText "Topic" $ pure . parseTopic

instance ToJSON Topic where
  toJSON t = toJSON (unTopic t)

parseTopic :: Text -> Topic
parseTopic t =
  case mkTopic t of
    Just topic' -> topic'
    -- TODO I guess I should probably throw here or something? I
    -- dunno. I need to at least alert the user that a given topic has
    -- something wrong with it
    Nothing -> fromJust . mkTopic $ "failedToMakeTopic"
