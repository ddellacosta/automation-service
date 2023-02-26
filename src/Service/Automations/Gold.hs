module Service.Automations.Gold
  ( goldAutomation
  , gledoptoLightStrip
  ,
  )
where

import Prelude hiding (id, init)

import Control.Lens ((^?))
import Control.Monad.Reader (MonadReader, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Aeson.Lens (key)
import qualified Data.Text as T
import Network.MQTT.Client (Topic)
import Service.App (Logger(..), MonadMQTT(..))
import qualified Service.App.Helpers as Helpers
import Service.Automation as Automation
import Service.AutomationName (AutomationName(..))
import Service.Device (DeviceId)
import Service.Env (Env)
import Service.Messages.GledoptoController
  ( Effect(..)
  , effect'
  , hex'
  , mkHex
  , seconds
  , withTransition'
  )
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM (TChan, atomically, tryReadTChan)

gledoptoLightStrip :: DeviceId
gledoptoLightStrip = "0xb4e3f9fffe14c707"

goldAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => Automation m
goldAutomation =
  Automation
    { _name = Gold
    , _devices = [gledoptoLightStrip]
    , _wantsFullControlOver = [gledoptoLightStrip]
    , _cleanup = cleanupAutomation
    , _run = runAutomation
    }

cleanupAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => TChan Automation.Message
  -> m ()
cleanupAutomation _broadcastChan = do
  info $ "Shutting down Gold"

  lightStripTopic <- Helpers.findDeviceTopicM gledoptoLightStrip

  info "turning led strip off"
  publishMQTT lightStripTopic "{\"state\": \"OFF\"}"

runAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => TChan Automation.Message
  -> m ()
runAutomation broadcastChan = do
  info "Running Gold"

  lightStripTopic <- Helpers.findDeviceTopicM gledoptoLightStrip

  debug $ "topic? " <> (T.pack $ show lightStripTopic)

  debug "turning on"
  publishMQTT lightStripTopic "{\"state\": \"ON\"}"

  debug "setting color to orange"
  publishMQTT lightStripTopic (hex' "be9fc1")

  liftIO $ threadDelay (seconds 2)

  debug "setting color to pink with 3 second transition"
  publishMQTT lightStripTopic (withTransition' 3 $ mkHex "F97C00")

  debug "starting breathe loop"
  go lightStripTopic broadcastChan

  where
    go
      :: (Logger m, MonadReader Env m, MonadMQTT m, MonadUnliftIO m)
      => Topic
      -> TChan Automation.Message
      -> m ()
    go lightStripTopic broadcastChan' = do
      liftIO $ threadDelay $ seconds 60
      debug "Gold: breathe"
      publishMQTT lightStripTopic $ effect' Breathe
      --
      -- For now, this and GoldMsg below are just here to remind me
      -- how to create and use per-Automation message types...actually,
      -- testing it just now this doesn't seem to pick up messages
      -- anyways. TODO: fix
      --
      maybeMsg <- atomically $ tryReadTChan broadcastChan'
      debug $ "Gold got msg? " <> T.pack (show maybeMsg)
      case maybeMsg of
        Just (Client Gold msg') ->
          debug
            ( T.pack $
                "Gold received msg. Mood is " <>
                show (msg' ^? key "mood") <>
                ", Fancy = " <>
                show (msg' ^? key "fancy")
            )
            >> go lightStripTopic broadcastChan'
        _ -> go lightStripTopic broadcastChan'
