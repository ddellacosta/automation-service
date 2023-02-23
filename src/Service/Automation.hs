module Service.Automation
  ( Automation(..)
  , Message(..)
  , devices
  , name
  , nullAutomation
  , wantsFullControlOver
  )
  where

import Control.Lens (Lens', lens)
import Data.Aeson (Value)
import Service.AutomationName (AutomationName(..))
import Service.Device (DeviceId)
import UnliftIO.STM (TChan)

data Message
  = Client AutomationName Value
  | Server AutomationName Value
  deriving (Show)

data Automation monad = Automation
  { _name :: AutomationName
  , _devices :: [DeviceId]
  , _wantsFullControlOver :: [DeviceId]
  , _cleanup :: TChan Message -> monad ()
  , _run :: TChan Message -> monad ()
  }

name :: Lens' (Automation m) AutomationName
name = lens _name (\am newName -> am { _name = newName })

devices :: Lens' (Automation m) [DeviceId]
devices =
  lens _devices (\am newDevices -> am { _devices = newDevices })

wantsFullControlOver :: Lens' (Automation m) [DeviceId]
wantsFullControlOver =
  lens _wantsFullControlOver $ \am newControlled ->
    am { _wantsFullControlOver = newControlled }

nullAutomation :: (Applicative m) => Automation m
nullAutomation = Automation Null [] [] noop noop
  where
    noop = const $ pure ()
