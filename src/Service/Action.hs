module Service.Action
  ( Action
  , ActionFor(..)
  , Message(..)
  , MsgBody(..)
  , nullAction
  )
  where

import Data.Text (Text)
import Service.ActionName (ActionName(..))
import Service.Device (DeviceId)
import UnliftIO.STM (TChan)

-- TODO need to fix these messaging types to not be garbage
data Message = Server MsgBody | Client MsgBody
  deriving (Show)

data MsgBody = MsgBody
  { msg :: Text
  }
  deriving (Show)

data ActionFor monad msg = ActionFor
  { name :: ActionName
  , devices :: [DeviceId]
  , wantsFullControlOver :: [DeviceId]
  , cleanup :: TChan msg -> monad ()
  , run :: TChan msg -> monad ()
  }

type Action m = ActionFor m Message

nullAction :: (Applicative m) => ActionFor m a
nullAction = ActionFor Null [] [] noop noop
  where
    noop = const $ pure ()
