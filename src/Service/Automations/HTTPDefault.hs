module Service.Automations.HTTPDefault
  ( defaultHttpAutomation
  )
where

import Control.Lens (view)
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.Reader (MonadReader)
import Data.Time.Clock (UTCTime)
import Service.App (Logger, debug)
import qualified Service.Automation as Automation
import Service.Automation (Automation (..))
import qualified Service.AutomationName as AutomationName
import Service.Env (Env, config, daemonBroadcast, httpPort)
import Service.MQTT.Class (MQTTClient)
import qualified Service.MQTT.Messages.Daemon as Daemon
import UnliftIO.STM (TChan, atomically, writeTChan)

defaultHttpAutomation
  :: (Logger l, MQTTClient mc, MonadReader (Env l mc) m, MonadUnliftIO m)
  => UTCTime
  -> Automation m
defaultHttpAutomation ts =
  Automation
    { _name = AutomationName.HTTPDefault
    , _cleanup = mkCleanupAutomation
    , _run = mkRunAutomation
    , _startTime = ts
    }


mkCleanupAutomation
  :: (Logger l, MQTTClient mc, MonadReader (Env l mc) m, MonadUnliftIO m)
  => (TChan Automation.Message -> m ())
mkCleanupAutomation = \_broadcastChan -> do
  debug $ "Starting Cleanup: HTTPDefault"


mkRunAutomation
  :: (Logger l, MQTTClient mc, MonadReader (Env l mc) m, MonadUnliftIO m)
  => (TChan Automation.Message -> m ())
mkRunAutomation = \_broadcastChan -> do
  debug $ "Beginning run of HTTPDefault"
  daemonBroadcast' <- view daemonBroadcast
  defaultPort <- view $ config . httpPort
  atomically $ writeTChan daemonBroadcast' (Daemon.Start (AutomationName.HTTP defaultPort))
