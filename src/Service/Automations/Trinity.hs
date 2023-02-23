module Service.Automations.Trinity
  ( trinityAutomation
  ,
  )
where

import Control.Monad (forever)
import Control.Monad.Reader (MonadReader)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Network.MQTT.Client (Topic)
import Service.App (Logger(..), MonadMQTT(..))
import qualified Service.App.Helpers as Helpers
import Service.Automation (Automation(..), Message(..))
import Service.AutomationName (AutomationName(..))
import qualified Service.Device as Device
import Service.Env (Env')
import Service.Messages.GledoptoController (mkColorXY, seconds, withTransition')
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM (TChan)

trinityAutomation
  :: (Logger m, MonadMQTT m, MonadReader (Env' logger mqttClient) m, MonadUnliftIO m)
  => Automation m
trinityAutomation =
  Automation
    { _name = Trinity
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
  info "Shutting down Trinity"

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
  info "Running Trinity"

  -- TODO FAIL APPROPRIATELY, LOG IT, AND STOP THREAD IF WE CAN'T LOAD THE DEVICE
  -- (_gledoptoLedStrip, ledTopic) <- Helpers.findDeviceM Device.GledoptoGLC007P_1

  debug "turning on"
  publishMQTT ledTopic "{\"state\": \"ON\"}"

  publishMQTT ledTopic (withTransition' 3 $ mkColorXY 0.7 0.28)

  forever $ do
    threadDelay (10 `seconds`)
    publishMQTT ledTopic (withTransition' 3 $ mkColorXY 0.1 0.12)
    threadDelay (10 `seconds`)
    publishMQTT ledTopic (withTransition' 3 $ mkColorXY 0.83 0.75)
    threadDelay (10 `seconds`)
    publishMQTT ledTopic (withTransition' 3 $ mkColorXY 0.7 0.28)
