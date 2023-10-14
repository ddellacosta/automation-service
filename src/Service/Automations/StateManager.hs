{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Service.Automations.StateManager
  ( stateManagerAutomation
  ,
  )
where

import Prelude hiding (id, init)

import Control.Lens (view)
import Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import Control.Monad.Reader (MonadReader)
import qualified Data.Aeson as Aeson
import Data.Time.Clock (UTCTime)
import qualified Data.Vector as V
import Service.App (Logger (..), info)
import qualified Service.Automation as Automation
import Service.Automation (Automation (..))
import Service.AutomationName (AutomationName (..))
import Service.Env (Env, config, dbPath)
import Service.MQTT.Class (MQTTClient (..))
import qualified Service.StateStore as StateStore
import UnliftIO.STM (TChan, atomically, readTChan)

stateManagerAutomation
  :: (Logger l, MQTTClient mc, MonadReader (Env l mc) m, MonadUnliftIO m)
  => UTCTime
  -> Automation m
stateManagerAutomation ts =
  Automation
    { _name = StateManager
    , _cleanup = cleanupAutomation
    , _run = runAutomation
    , _startTime = ts
    }

cleanupAutomation
  :: (Logger l, MQTTClient mc, MonadReader (Env l mc) m, MonadUnliftIO m)
  => TChan Automation.Message
  -> m ()
cleanupAutomation _broadcastChan = do
  info "Shutting down StateManager"

runAutomation
  :: (Logger l, MQTTClient mc, MonadReader (Env l mc) m, MonadUnliftIO m)
  => TChan Automation.Message
  -> m ()
runAutomation broadcastChan = do
  info "Running StateManager"
  dbPath' <- view $ config . dbPath
  go dbPath'

  where
    go
      :: (Logger l, MonadReader (Env l mc) m, MQTTClient mc, MonadUnliftIO m)
      => FilePath
      -> m ()
    go dbPath' = do
      msg <- atomically . readTChan $ broadcastChan

      case msg of
        Automation.Client StateManager (Automation.ValueMsg (Aeson.Array runningAutos)) -> do
          liftIO $
            StateStore.updateRunning dbPath' . V.toList $
              (\(Aeson.String t) -> t) <$> runningAutos
          go dbPath'

        Automation.Client StateManager (Automation.ByteStringMsg msgs) -> do
          liftIO $ StateStore.updateScheduled dbPath' msgs
          go dbPath'

        _ -> go dbPath'
