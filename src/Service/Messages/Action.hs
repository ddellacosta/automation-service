{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Service.Messages.Action
  ( Action(..)
  ,
  )
where

import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value
  , (.:?)
  , defaultOptions
  , genericToEncoding
  , withObject
  )
import Data.Aeson.Types (Parser)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Service.ActionName (ActionName, parseActionName)

data Action msg where
  StopServer :: Action msg
  Start :: ActionName -> Action msg
  Stop :: ActionName -> Action msg
  SendTo :: ActionName -> msg -> Action msg
  Null :: Action msg
  deriving (Generic, Eq, Ord, Show)

--
-- TODO generalize this for different msg types
--
instance (ToJSON msgBody) => ToJSON (Action msgBody) where
  toEncoding = genericToEncoding defaultOptions

instance (FromJSON msgBody) => FromJSON (Action msgBody) where
  --
  -- The semantics of this are pretty stupid. Basically anything other
  -- than a perfectly well-formed message with a single action should
  -- be considered to provoke undefined behavior.
  --
  -- In reality, this will favor properties that come earlier in the
  -- `case` statement below, e.g. if you pass in a JSON value of
  --
  --   `{"stop": "MyFirstAction", "start": "MySecondAction"}
  --
  -- it will consistently start `MySecondAction` and `MyFirstAction`
  -- will never be stopped.
  --
  -- This is largely because this was the easiest thing to
  -- implement. If it turns out we really want to be able to pass
  -- multiple actions in a single message, I'll revisit this.
  --
  parseJSON :: Value -> Parser (Action msgBody)
  parseJSON = withObject "Action" $ \o -> do
    startAction <- o .:? "start"
    stopAction <- o .:? "stop"
    sendTo <- o .:? "send"
    msg <- o .:? "msg"
    pure $ fromMaybe Null $ case (startAction, stopAction, sendTo) of
      (Just actionName, _, _) -> Start <$> parseActionName actionName
      (_, Just actionName, _) -> Stop <$> parseActionName actionName
      (_, _, Just actionName) -> do
        sendToAction <- parseActionName actionName
        SendTo sendToAction <$> msg
      _ -> Nothing
