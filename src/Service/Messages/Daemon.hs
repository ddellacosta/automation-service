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
  )
where

import Control.Lens (makePrisms)
import qualified Data.Aeson as Aeson
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
import Network.MQTT.Topic (Topic)
import Service.AutomationName (AutomationName, parseAutomationName)
import Service.Device (Device, DeviceId)
import UnliftIO.STM (TChan)

type AutomationSchedule = Text
type AutomationMessage = Value

data Message where
  Start :: AutomationName -> Message
  Stop :: AutomationName -> Message
  SendTo :: AutomationName -> AutomationMessage -> Message
  Schedule :: Message -> AutomationSchedule -> Message
  DeviceUpdate :: [Device] -> Message
  Register :: DeviceId -> AutomationName -> Message
  Subscribe :: Maybe Topic -> TChan Value -> Message
  Null :: Message
  deriving (Generic, Eq) -- , Show)

instance Show Message where
  show = \case
    Start automationName -> "Start " <> show automationName
    Stop automationName -> "Stop " <> show automationName
    SendTo automationName msg -> "SendTo " <> show automationName <> " " <> show msg
    Schedule msg schedule -> "Schedule " <> show msg <> " " <> show schedule
    DeviceUpdate devices -> "DeviceUpdate " <> show devices
    Register deviceId automationName ->
      "Register " <> show deviceId <> " " <> show automationName
    Subscribe mTopic _automationListenerChannel ->
      "Subscribe " <> show mTopic <> ", with listener channel"
    Null -> "Null"

makePrisms ''Message

instance ToJSON Message where
  -- at the moment I don't care what this produces, because this thing
  -- doesn't emit Message values as JSON for any reason. Right now I
  -- can't imagine why it ever would, but at that time I can implement
  -- something. Right now I just want it to work with the TChan Value
  -- in Subscribe because it keeps the design cleaner.
  toJSON _ = Aeson.String "Message"

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
