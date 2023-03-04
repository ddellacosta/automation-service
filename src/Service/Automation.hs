module Service.Automation
  ( Automation(..)
  , Message(..)
  , name
  , nullAutomation
  )
  where

import Control.Lens (Lens', lens)
import Data.Aeson (Value)
import Service.AutomationName (AutomationName(..))
import UnliftIO.STM (TChan)

data Message
  = Client AutomationName Value
  | Server AutomationName Value
  deriving (Show)

data Automation monad = Automation
  { _name :: AutomationName
  , _cleanup :: TChan Message -> monad ()
  , _run :: TChan Message -> monad ()
  }

name :: Lens' (Automation m) AutomationName
name = lens _name (\am newName -> am { _name = newName })

nullAutomation :: (Applicative m) => Automation m
nullAutomation = Automation Null noop noop
  where
    noop = const $ pure ()
