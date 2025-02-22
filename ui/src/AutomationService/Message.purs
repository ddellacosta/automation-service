module AutomationService.Message
 ( Message(..)
 , Page(..)
 , pageName
 , pageNameClass
 )
where

import Prelude

import AutomationService.DeviceMessage as Devices
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.Common as S
import Data.String.Pattern (Pattern(..), Replacement(..))

data Message ws
  = SetPage Page
  | DeviceMsg Devices.Message
  | InitWS ws
  | PublishMsgChanged String
  | Publish

data Page
  = Devices
  | Groups
  | PublishMQTT

derive instance Generic Page _

instance Show Page where
  show = genericShow

pageName :: Page -> String
pageName = case _ of
  Devices -> "Devices"
  Groups -> "Groups"
  PublishMQTT -> "Publish MQTT"

pageNameClass :: Page -> String
pageNameClass =
      pageName
  >>> S.toLower
  >>> S.trim
  >>> S.replaceAll (Pattern " ") (Replacement "-")
