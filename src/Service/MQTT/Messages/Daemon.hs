{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Service.MQTT.Messages.Daemon
  ( AutomationSchedule
  , JobId
  , Message(..)
  , _DeviceUpdate
  , _GroupUpdate
  , _Null
  , _RegisterDevice
  , _RegisterGroup
  , _Schedule
  , _SendTo
  , _Start
  , _Stop
  , _Subscribe
  , _Unschedule
  )
where

import Control.Lens (makePrisms)
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON(..), ToJSON(..), Value, (.:?), withObject)
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Network.MQTT.Topic (Topic)
import Service.AutomationName (AutomationName, parseAutomationName)
import Service.Device (Device, DeviceId)
import Service.Group (Group, GroupId)
import UnliftIO.STM (TChan)

type AutomationMessage = Value
type AutomationSchedule = Text
type JobId = Text

data Message where
  Start :: AutomationName -> Message
  Stop :: AutomationName -> Message
  SendTo :: AutomationName -> AutomationMessage -> Message
  Schedule :: JobId -> AutomationSchedule -> Message -> Message
  Unschedule :: JobId -> Message
  DeviceUpdate :: [Device] -> Message
  GroupUpdate :: [Group] -> Message
  RegisterDevice :: DeviceId -> AutomationName -> Message
  RegisterGroup :: GroupId -> AutomationName -> Message
  Subscribe :: AutomationName -> Maybe Topic -> TChan Value -> Message
  Status :: Message
  Null :: Message
  deriving (Generic, Eq)

makePrisms ''Message

--
-- This is tedious but it's not a big deal compared to having to do
-- some other nonsense to independently pass the TChan Value around so
-- that I can automatically generate a Show and ToJSON instance for
-- Message. Anyways, Show instances are only used in logging.
--
-- Also see the comment for ToJSON below.
--
instance Show Message where
  show = \case
    Start automationName -> "Start " <> show automationName
    Stop automationName -> "Stop " <> show automationName
    SendTo automationName msg ->
      "SendTo " <> show automationName <> " " <> show msg
    Schedule jobId schedule msg ->
      "Schedule " <> " " <> show jobId <> " " <> show schedule <> show msg
    Unschedule jobId -> "Unschedule " <> show jobId
    DeviceUpdate devices -> "DeviceUpdate " <> show devices
    GroupUpdate groups -> "GroupUpdate " <> show groups
    RegisterDevice deviceId automationName ->
      "RegisterDevice " <> show deviceId <> " " <> show automationName
    RegisterGroup groupId automationName ->
      "RegisterGroup " <> show groupId <> " " <> show automationName
    Subscribe automationName mTopic _automationListenerChannel ->
      "Subscribe " <> show automationName <> " " <> show mTopic <> ", with listener channel"
    Status -> "Status"
    Null -> "Null"

instance ToJSON Message where
  --
  -- At the moment I don't care what this produces, because this thing
  -- doesn't emit Message values as JSON for any reason. Right now I
  -- can't imagine why it ever would, but at that time I can implement
  -- something. Now I just want it to work with the TChan Value in
  -- Subscribe because it keeps the design cleaner.
  --
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
    schedule <- o .:? "schedule"
    cron <- o .:? "cron"
    jobId <- o .:? "jobId"
    unschedule <- o .:? "unschedule"
    pure $
      fromMaybe Null $
        case (startAutomation, stopAutomation, sendTo, schedule, cron, jobId, unschedule) of
          (Just automationName, _, _, _, _, _, _) -> Start <$> parseAutomationName automationName
          (_, Just automationName, _, _, _, _, _) -> Stop <$> parseAutomationName automationName
          (_, _, Just automationName, _, _, _, _) -> do
            sendToAutomation <- parseAutomationName automationName
            SendTo sendToAutomation <$> msg
          (_, _, _, Just msg', Just (Aeson.String cron'), Just (Aeson.String jobId'), _) ->
            Schedule jobId' cron' <$> msg'
          (_, _, _, _, _, _, Just (Aeson.String jobId')) -> Just (Unschedule jobId')
          _ -> Nothing
