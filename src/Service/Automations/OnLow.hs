module Service.Automations.OnLow
  ( onLowAutomation
  ,
  )
where

import Control.Monad (forever)
import Control.Monad.Reader (MonadReader)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Service.App (Logger(..), MonadMQTT(..))
import qualified Service.App.Helpers as Helpers
import Service.Automation (Automation(..), Message(..))
import Service.AutomationName (AutomationName(..))
import Service.Device (DeviceId)
import Service.Env (Env)
import Service.Messages.GledoptoController (mkColorXY, seconds, withTransition')
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM (TChan)

gledoptoLightStrip :: DeviceId
gledoptoLightStrip = "0x680ae2fffe4577ac"

onLowAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => Automation m
onLowAutomation =
  Automation
    { _name = OnLow
    , _devices = [gledoptoLightStrip]
    , _wantsFullControlOver = [gledoptoLightStrip]
    , _cleanup = cleanupAutomation
    , _run = runAutomation
    }

cleanupAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => TChan Message
  -> m ()
cleanupAutomation _broadcastChan = do
  info "Shutting down OnLow"

  lightStripTopic <- Helpers.findDeviceTopicM gledoptoLightStrip

  info "turning led strip off"
  publishMQTT lightStripTopic "{\"state\": \"OFF\"}"

runAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => TChan Message
  -> m ()
runAutomation _broadcastChan = do
  info "Running OnLow"

  lightStripTopic <- Helpers.findDeviceTopicM gledoptoLightStrip

  debug "turning on"
  publishMQTT lightStripTopic "{\"state\": \"ON\"}"

  publishMQTT lightStripTopic (withTransition' 3 $ mkColorXY 0.7 0.28)

  forever $ do
--     threadDelay (10 `seconds`)
--     publishMQTT lightStripTopic (withTransition' 3 $ mkColorXY 0.1 0.12)
    threadDelay (10 `seconds`)
    publishMQTT lightStripTopic (withTransition' 3 $ mkColorXY 0.83 0.75)
    threadDelay (10 `seconds`)
    publishMQTT lightStripTopic (withTransition' 3 $ mkColorXY 0.7 0.28)
