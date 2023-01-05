module Service.Action
  ( Action(..)
  , Message(..)
  , MsgBody(..)
  , mkNullAction
  , nullAction
  )
  where

import Data.Text (Text)
import qualified Data.UUID as UUID
import Data.UUID (UUID)
import Service.ActionName (ActionName(..))
import Service.App (ActionsService)
import Service.Device (DeviceId)

data Message = Server MsgBody | Client MsgBody
  deriving (Show)

data MsgBody = MsgBody
  { msg :: Text
  }
  deriving (Show)

data Action a = Action
  { name :: ActionName
  , id :: UUID
  , devices :: [DeviceId]
  , wantsFullControlOver :: [DeviceId]
  , init :: Text -> a -> ActionsService a
  , cleanup :: Text -> a -> ActionsService ()
  , run :: Text -> a -> ActionsService ()
  }

nullAction :: UUID -> Action a
nullAction newId = Action Null newId [] [] constA noop noop
  where
    constA _ = pure
    noop _ _ = pure ()

mkNullAction :: Action a
mkNullAction = nullAction UUID.nil
