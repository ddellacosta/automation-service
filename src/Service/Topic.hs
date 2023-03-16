{-# OPTIONS_GHC -fno-warn-orphans #-}

module Service.Topic
  ( parseTopic
  )
  where

import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , withText
  )
import Data.Maybe (fromJust)
import Data.Text (Text)
import Network.MQTT.Topic (Topic(..), mkTopic)

instance FromJSON Topic where
  parseJSON = withText "Topic" $ pure . parseTopic

instance ToJSON Topic where
  toJSON t = toJSON (unTopic t)

parseTopic :: Text -> Topic
parseTopic t =
  case mkTopic t of
    Just topic' -> topic'
    Nothing -> fromJust . mkTopic $ "failedToMakeTopic"
