module Service.Automations.StateManager
  ( stateManagerAutomation
  ,
  )
where

import Prelude hiding (id, init)

import Control.Lens (view)
import Control.Monad.Reader (MonadReader)
import Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import qualified Data.Aeson as Aeson
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Vector as V
import Service.App (Logger(..), MonadMQTT(..))
import Service.Automation as Automation
import Service.AutomationName (AutomationName(..), parseAutomationNameText)
import Service.Env (Env, config, daemonBroadcast, dbPath)
import qualified Service.StateStore as StateStore
import qualified Service.Messages.Daemon as Daemon
import UnliftIO.STM (TChan, atomically, readTChan, writeTChan)

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
  info "Shutting down StateManager"

runAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => TChan Automation.Message
  -> m ()
runAutomation broadcastChan = do
  info "Running StateManager"

  daemonBroadcast' <- view daemonBroadcast
  dbPath' <- view $ config . dbPath

  go broadcastChan dbPath'

  where
    go
      :: (Logger m, MonadReader Env m, MonadMQTT m, MonadUnliftIO m)
      => TChan Automation.Message
      -> FilePath
      -> m ()
    go broadcastChan' dbPath' = do
      msg <- atomically . readTChan $ broadcastChan'
      case msg of
        Client StateManager (Aeson.Array runningAutos) -> do
          liftIO $
            StateStore.updateRunning dbPath' . V.toList $ (\(Aeson.String t) -> t) <$> runningAutos
          go broadcastChan' dbPath'
        _ -> go broadcastChan' dbPath'
