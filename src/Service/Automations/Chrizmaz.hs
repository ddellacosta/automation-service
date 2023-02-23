module Service.Automations.Chrizmaz
  ( chrizmazAutomation
  ,
  )
  where

import Control.Monad (forever)
import Control.Monad.Reader (MonadReader)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Network.MQTT.Client (Topic)
import Service.AutomationName (AutomationName(..))
import Service.Automation (Automation(..), Message)
import Service.App (Logger(..), MonadMQTT(..))
import qualified Service.App.Helpers as Helpers
import Service.Messages.GledoptoController (Effect(..), effect', hex', seconds)
import qualified Service.Device as Device
import Service.Env (Env')
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM (TChan)

chrizmazAutomation
  :: (Logger m, MonadMQTT m, MonadReader (Env' logger mqttClient) m, MonadUnliftIO m)
  => Automation m
chrizmazAutomation =
  Automation
    { _name = Chrizmaz
    , _devices = []
    , _wantsFullControlOver = []
    , _cleanup = cleanupAutomation
    , _run = runAutomation
    }

ledTopic :: Topic
ledTopic = "zigbee2mqtt/Gledopto GL-C-007P RGBW LED Controller Pro/set" 

cleanupAutomation
  :: (Logger m, MonadMQTT m, MonadReader (Env' logger mqttClient) m, MonadUnliftIO m)
  => TChan Message
  -> m ()
cleanupAutomation _broadcastChan = do
  info "Shutting down Chrizmaz"

  -- TODO FAIL APPROPRIATELY, LOG IT, AND STOP THREAD IF WE CAN'T LOAD THE DEVICE
  -- if gledoptoLedStrip = nullDevice then throwException and quit
  -- (_gledoptoLedStrip, ledTopic) <- Helpers.findDeviceM Device.GledoptoGLC007P_1

  info "turning led strip off"
  publishMQTT ledTopic "{\"state\": \"OFF\"}"

runAutomation
  :: (Logger m, MonadMQTT m, MonadReader (Env' logger mqttClient) m, MonadUnliftIO m)
  => TChan Message
  -> m ()
runAutomation _broadcastChan = do
  info "Urnning Chrizmaz arbhft, Jolly HO!"

  -- TODO FAIL APPROPRIATELY, LOG IT, AND STOP THREAD IF WE CAN'T LOAD THE DEVICE
  -- if gledoptoLedStrip = nullDevice then throwException and quit
  -- (_gledoptoLedStrip, ledTopic) <- Helpers.findDeviceM Device.GledoptoGLC007P_1

  debug "turning on"
  publishMQTT ledTopic "{\"state\": \"ON\"}"

  forever $ do
    threadDelay (seconds 3)
    publishMQTT ledTopic (hex' "009900")
    publishMQTT ledTopic (effect' Breathe)
    threadDelay (seconds 3)
    publishMQTT ledTopic (hex' "CC0000")
    publishMQTT ledTopic (effect' Breathe)
