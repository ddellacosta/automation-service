{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Service.MQTT.Messages.Daemon
  ( AutomationSchedule
  , JobId
  , Message(..)
  , MQTTMessage(..)
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
import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.MQTT.Topic (Topic)
import Service.Automation (ClientMsg)
import Service.AutomationName (AutomationName, parseAutomationName)
import Service.Device (Device, DeviceId)
import Service.Group (Group, GroupId)
import UnliftIO.STM (TChan)

type AutomationSchedule = Text
type JobId = Text

data MQTTMessage = MQTTMsg
  { topic :: Topic
  , msg :: Value
  }
  deriving (Generic, Eq, Show)

data Message where
  Start :: AutomationName -> Message
  Stop :: AutomationName -> Message
  SendTo :: AutomationName -> ClientMsg -> Message
  Schedule :: JobId -> AutomationSchedule -> Message -> Message
  Unschedule :: JobId -> Message
  DeviceUpdate :: [Device] -> ByteString -> Message
  GroupUpdate :: [Group] -> ByteString -> Message
  RegisterDevice :: DeviceId -> AutomationName -> Message
  RegisterGroup :: GroupId -> AutomationName -> Message
  DeRegisterDevicesAndGroups :: AutomationName -> Message
  DeadAutoCleanup :: Message
  Subscribe :: AutomationName -> Maybe Topic -> TChan Value -> Message
  Status :: Message
  Publish :: MQTTMessage -> Message
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
      "Schedule " <> " " <> show jobId <> " " <> show schedule <> " " <> show msg
    Unschedule jobId -> "Unschedule " <> show jobId
    -- todo: show truncated msg for these two
    DeviceUpdate devices _msg -> "DeviceUpdate " <> show devices
    GroupUpdate groups _msg -> "GroupUpdate " <> show groups
    RegisterDevice deviceId automationName ->
      "RegisterDevice " <> show deviceId <> " " <> show automationName
    RegisterGroup groupId automationName ->
      "RegisterGroup " <> show groupId <> " " <> show automationName
    DeRegisterDevicesAndGroups automationName ->
      "DeRegisterDevicesAndGroups " <> show automationName
    DeadAutoCleanup -> "DeadAutoCleanup"
    Subscribe automationName mTopic _automationListenerChannel ->
      "Subscribe " <> show automationName <> " " <> show mTopic <> ", with listener channel"
    Publish (MQTTMsg topic msg) -> "Publish to " <> show topic <> ": " <> show msg
    Status -> "Status"
    Null -> "Null"

instance ToJSON Message where
  toJSON (Start autoName) = object [ "start" .= autoName ]
  toJSON (Stop autoName) = object [ "stop" .= autoName ]
  toJSON (SendTo autoName clientMsg) = object
    [ "send" .= autoName
    , "msg" .= clientMsg
    ]
  toJSON (Schedule jobId sched jobMsg) = object
    [ "jobId" .= jobId
    , "schedule" .= sched
    , "job" .= jobMsg
    ]
  toJSON (Unschedule jobId) = object [ "unschedule" .= jobId ]
  toJSON Status = object [ "status" .= object [] ]
  toJSON _ = object []

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
    job <- o .:? "job"
    schedule <- o .:? "schedule"
    jobId <- o .:? "jobId"
    unschedule <- o .:? "unschedule"
    --
    -- This is meant to be JSON to pass along over MQTT (to
    -- `topic`). I think I want to just pass ByteString through
    -- without first decoding it as a Value, but I'm not yet sure how
    -- to do that here. Also, maybe it is better to ensure it is valid
    -- and fail here if not? I need to make this parsing smarter real
    -- soon now...
    --
    (publishMsg :: Maybe Value) <- o .:? "publish"
    (topic :: Maybe Topic) <- o .:? "topic"
    pure $
      fromMaybe Null $
        case (startAutomation, stopAutomation, sendTo, jobId, schedule, unschedule, publishMsg, topic) of
          (Just automationName, _, _, _, _, _, _, _) ->
            Start <$> parseAutomationName automationName
          (_, Just automationName, _, _, _, _, _, _) ->
            Stop <$> parseAutomationName automationName
          (_, _, Just automationName, _, _, _, _, _) -> do
            sendToAutomation <- parseAutomationName automationName
            SendTo sendToAutomation <$> msg
          (_, _, _, Just (Aeson.String jobId'), Just (Aeson.String schedule'),  _, _, _) ->
            Schedule jobId' schedule' <$> job
          (_, _, _, _, _, Just (Aeson.String jobId'), _, _) ->
            Just (Unschedule jobId')
          (_, _, _, _, _, _, Just publishMsg', Just topic') ->
            Just (Publish (MQTTMsg topic' publishMsg'))
          _ -> Nothing
