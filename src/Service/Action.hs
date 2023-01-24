module Service.Action
  ( Action
  , ActionFor(..)
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
import Service.Device (DeviceId)
import UnliftIO.STM (TChan)

-- TODO need to fix these messaging types to not be garbage
data Message = Server MsgBody | Client MsgBody
  deriving (Show)

data MsgBody = MsgBody
  { msg :: Text
  }
  deriving (Show)

data ActionFor m a = ActionFor
  { name :: ActionName
  , id :: UUID
  , devices :: [DeviceId]
  , wantsFullControlOver :: [DeviceId]
  , init :: Text -> a -> m a
  , cleanup :: Text -> a -> m ()
  , run :: Text -> a -> m ()
  }

type Action m = ActionFor m (TChan Message)

nullAction :: (Applicative m) => UUID -> ActionFor m a
nullAction newId = ActionFor Null newId [] [] constA noop noop
  where
    constA _ = pure
    noop _ _ = pure ()

mkNullAction :: (Applicative m) => ActionFor m a
mkNullAction = nullAction UUID.nil
