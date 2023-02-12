module Service.Action
  ( Action
  , ActionFor(..)
  , Message(..)
  , nullAction
  )
  where

import Data.Aeson (FromJSON)
import Service.ActionName (ActionName(..))
import Service.Device (DeviceId)
import UnliftIO.STM (TChan)

data Message where
  Server :: forall e. (FromJSON e, Show e) => e -> Message
  Client :: forall e. (FromJSON e, Show e) => e -> Message

data ActionFor monad = ActionFor
  { name :: ActionName
  , devices :: [DeviceId]
  , wantsFullControlOver :: [DeviceId]
  , cleanup :: TChan Message -> monad ()
  , run :: TChan Message -> monad ()
  }

type Action m = ActionFor m

nullAction :: (Applicative m) => ActionFor m
nullAction = ActionFor Null [] [] noop noop
  where
    noop = const $ pure ()
