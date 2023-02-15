module Service.Actions.Chrizmaz
  ( chrizmazAction
  ,
  )
  where

import Control.Monad (forever)
import Control.Monad.Reader (MonadReader)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Service.ActionName (ActionName(..))
import Service.Action (Action(..), Message)
import Service.App (Logger(..), MonadMQTT(..))
import qualified Service.App.Helpers as Helpers
import Service.Messages.GledoptoGLC007P (Effect(..), effect', hex', seconds)
import qualified Service.Device as Device
import Service.Env (Env')
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM (TChan)

chrizmazAction
  :: (Logger m, MonadMQTT m, MonadReader (Env' logger mqttClient) m, MonadUnliftIO m)
  => Action m
chrizmazAction =
  Action
    { _name = Chrizmaz
    , _devices = [Device.GledoptoGLC007P_1]
    , _wantsFullControlOver = [Device.GledoptoGLC007P_1]
    , _cleanup = cleanupAction
    , _run = runAction
    }

cleanupAction
  :: (Logger m, MonadMQTT m, MonadReader (Env' logger mqttClient) m, MonadUnliftIO m)
  => TChan Message
  -> m ()
cleanupAction _broadcastChan = do
  info "Shutting down Chrizmaz"

  -- TODO FAIL APPROPRIATELY, LOG IT, AND STOP THREAD IF WE CAN'T LOAD THE DEVICE
  -- if gledoptoLedStrip = nullDevice then throwException and quit
  (_gledoptoLedStrip, ledTopic) <- Helpers.findDeviceM Device.GledoptoGLC007P_1

  info "turning led strip off"
  publishMQTT ledTopic "{\"state\": \"OFF\"}"

runAction
  :: (Logger m, MonadMQTT m, MonadReader (Env' logger mqttClient) m, MonadUnliftIO m)
  => TChan Message
  -> m ()
runAction _broadcastChan = do
  info "Urnning Chrizmaz arbhft, Jolly HO!"

  -- TODO FAIL APPROPRIATELY, LOG IT, AND STOP THREAD IF WE CAN'T LOAD THE DEVICE
  -- if gledoptoLedStrip = nullDevice then throwException and quit
  (_gledoptoLedStrip, ledTopic) <- Helpers.findDeviceM Device.GledoptoGLC007P_1

  debug "turning on"
  publishMQTT ledTopic "{\"state\": \"ON\"}"

  forever $ do
    threadDelay (seconds 3)
    publishMQTT ledTopic (hex' "009900")
    publishMQTT ledTopic (effect' Breathe)
    threadDelay (seconds 3)
    publishMQTT ledTopic (hex' "CC0000")
    publishMQTT ledTopic (effect' Breathe)
