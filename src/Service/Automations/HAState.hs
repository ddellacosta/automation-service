module Service.Automations.HAState
  ( haStateAutomation
  ,
  )
where

import Prelude hiding (id, init)

import Control.Monad.Reader (MonadReader) --, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Time.Clock (UTCTime)
import Service.App (Logger(..), MonadMQTT(..))
import qualified Service.Automation as Automation
import Service.Automation (Automation(..))
import Service.AutomationName (AutomationName(..))
import Service.Env (Env)
import UnliftIO.STM (TChan)

haStateAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => UTCTime
  -> Automation m
haStateAutomation ts =
  Automation
    { _name = HAState
    , _cleanup = cleanupAutomation
    , _run = runAutomation
    , _startTime = ts
    }

cleanupAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => TChan Automation.Message
  -> m ()
cleanupAutomation _broadcastChan = do
  info $ "Shutting down HAState"

runAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => TChan Automation.Message
  -> m ()
runAutomation _broadcastChan = do
  info "Running HAState"
