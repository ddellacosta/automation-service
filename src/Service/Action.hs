module Service.Action
  ( Action(..)
  , Message(..)
  , devices
  , name
  , nullAction
  , wantsFullControlOver
  )
  where

import Control.Lens (Lens', lens)
import Data.Aeson (Value)
import Service.ActionName (ActionName(..))
import Service.Device (DeviceId)
import UnliftIO.STM (TChan)

data Message
  = Client ActionName Value
  | Server ActionName Value
  deriving (Show)

data Action monad = Action
  { _name :: ActionName
  , _devices :: [DeviceId]
  , _wantsFullControlOver :: [DeviceId]
  , _cleanup :: TChan Message -> monad ()
  , _run :: TChan Message -> monad ()
  }

name :: Lens' (Action m) ActionName
name = lens _name (\am newName -> am { _name = newName })

devices :: Lens' (Action m) [DeviceId]
devices =
  lens _devices (\am newDevices -> am { _devices = newDevices })

wantsFullControlOver :: Lens' (Action m) [DeviceId]
wantsFullControlOver =
  lens _wantsFullControlOver $ \am newControlled ->
    am { _wantsFullControlOver = newControlled }

nullAction :: (Applicative m) => Action m
nullAction = Action Null [] [] noop noop
  where
    noop = const $ pure ()
