module Service.Automations.StateManager
  ( stateManagerAutomation
  ,
  )
where

import Control.Monad.Reader (MonadReader)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Service.App (Logger(..), MonadMQTT(..))
import qualified Service.Automation as Automation
import Service.Automation (Automation(..))
import Service.AutomationName (AutomationName(..))
import Service.Env (Env)
-- import qualified Service.Messages.Daemon as Daemon
import UnliftIO.STM (TChan)

stateManagerAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => Automation m
stateManagerAutomation =
  Automation
    { _name = StateManager
    , _cleanup = cleanupAutomation
    , _run = runAutomation
    }

cleanupAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => TChan Automation.Message
  -> m ()
cleanupAutomation _broadcastChan = do
  info $ "Shutting down StateManagerAutomation"

runAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => TChan Automation.Message
  -> m ()
runAutomation _broadcastChan = do
  info "Running StateManagerAutomation"
