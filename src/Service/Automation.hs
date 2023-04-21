module Service.Automation
  ( Automation(..)
  , Message(..)
  , name
  , nullAutomation
  , startTime
  )
  where

import Control.Lens (Lens', lens)
import Data.Aeson (Value)
import Data.Time.Clock (UTCTime)
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
  , _startTime :: UTCTime
  }

name :: Lens' (Automation m) AutomationName
name = lens _name (\am newName -> am { _name = newName })

startTime :: Lens' (Automation m) UTCTime
startTime = lens _startTime (\am newStartTime -> am { _startTime = newStartTime })

nullAutomation :: (Applicative m) => UTCTime -> Automation m
nullAutomation ts = Automation Null noop noop ts
  where
    noop = const $ pure ()
