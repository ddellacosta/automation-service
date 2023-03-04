module Service.Automations.Chrizmaz
  ( chrizmazAutomation
  ,
  )
  where

import Control.Monad (forever)
import Control.Monad.Reader (MonadReader)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Service.AutomationName (AutomationName(..))
import Service.Automation (Automation(..), Message)
import Service.App (Logger(..), MonadMQTT(..))
import qualified Service.App.Helpers as Helpers
import Service.Messages.GledoptoController (Effect(..), effect', hex', seconds)
import Service.Device (DeviceId)
import Service.Env (Env)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM (TChan)

gledoptoLightStrip :: DeviceId
gledoptoLightStrip = "0xb4e3f9fffe14c707"

chrizmazAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => Automation m
chrizmazAutomation =
  Automation
    { _name = Chrizmaz
    , _cleanup = cleanupAutomation
    , _run = runAutomation
    }

cleanupAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => TChan Message
  -> m ()
cleanupAutomation _broadcastChan = do
  info "Shutting down Chrizmaz"

  lightStripTopic <- Helpers.findDeviceTopicM gledoptoLightStrip

  info "turning led strip off"
  publishMQTT lightStripTopic "{\"state\": \"OFF\"}"

runAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => TChan Message
  -> m ()
runAutomation _broadcastChan = do
  info "Urnning Chrizmaz arbhft, Jolly HO!"

  lightStripTopic <- Helpers.findDeviceTopicM gledoptoLightStrip

  debug "turning on"
  publishMQTT lightStripTopic "{\"state\": \"ON\"}"

  forever $ do
    threadDelay (seconds 3)
    publishMQTT lightStripTopic (hex' "009900")
    publishMQTT lightStripTopic (effect' Breathe)
    threadDelay (seconds 3)
    publishMQTT lightStripTopic (hex' "CC0000")
    publishMQTT lightStripTopic (effect' Breathe)
