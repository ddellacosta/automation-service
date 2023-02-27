{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Service.Messages.Daemon
  ( Message(..)
  , AutomationSchedule
  , _Null
  , _Schedule
  , _SendTo
  , _Start
  , _Stop
  , _StopServer
  )
where

import Control.Lens (makePrisms)
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
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Service.AutomationName (AutomationName, parseAutomationName)
import Service.Device (Device)

type AutomationSchedule = Text
type AutomationMessage = Value

data Message where
  StopServer :: Message
  Start :: AutomationName -> Message
  Stop :: AutomationName -> Message
  SendTo :: AutomationName -> AutomationMessage -> Message
  Schedule :: Message -> AutomationSchedule -> Message
  DeviceUpdate :: [Device] -> Message
  Null :: Message
  deriving (Generic, Eq, Show)

makePrisms ''Message

instance ToJSON Message where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Message where
  --
  -- The semantics of this are pretty stupid. Basically anything other
  -- than a perfectly well-formed message with a single automation should
  -- be considered to provoke undefined behavior.
  --
  -- In reality, this will favor properties that come earlier in the
  -- `case` statement below, e.g. if you pass in a JSON value of
  --
  --   `{"stop": "MyFirstAutomation", "start": "MySecondAutomation"}
  --
  -- it will consistently start `MySecondAutomation` and `MyFirstAutomation`
  -- will never be stopped.
  --
  -- This is largely because this was the easiest thing to
  -- implement. If it turns out we really want to be able to pass
  -- multiple automations in a single message, I'll revisit this.
  --
  parseJSON :: Value -> Parser Message
  parseJSON = withObject "Automation" $ \o -> do
    startAutomation <- o .:? "start"
    stopAutomation <- o .:? "stop"
    sendTo <- o .:? "send"
    msg <- o .:? "msg"
    pure $
      fromMaybe Null $
        case (startAutomation, stopAutomation, sendTo) of
          (Just automationName, _, _) -> Start <$> parseAutomationName automationName
          (_, Just automationName, _) -> Stop <$> parseAutomationName automationName
          (_, _, Just automationName) -> do
            sendToAutomation <- parseAutomationName automationName
            SendTo sendToAutomation <$> msg
          _ -> Nothing
