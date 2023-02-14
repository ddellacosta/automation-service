module Service.Action
  ( Action(..)
  , Message(..)
  , nullAction
  )
  where

import Data.Aeson (Value)
import Service.ActionName (ActionName(..))
import Service.Device (DeviceId)
import UnliftIO.STM (TChan)

data Message
  = Client ActionName Value
  | Server ActionName Value
  deriving (Show)

data Action monad = Action
  { name :: ActionName
  , devices :: [DeviceId]
  , wantsFullControlOver :: [DeviceId]
  , cleanup :: TChan Message -> monad ()
  , run :: TChan Message -> monad ()
  }

nullAction :: (Applicative m) => Action m
nullAction = Action Null [] [] noop noop
  where
    noop = const $ pure ()
