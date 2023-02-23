module Service.Automations.Gold
  ( goldAutomation
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
import Service.Automation (Automation(..), Message(..))
import Service.AutomationName (AutomationName(..))
import qualified Service.Device as Device
import Service.Env (Env')
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

goldAutomation
  :: (Logger m, MonadMQTT m, MonadReader (Env' logger mqttClient) m, MonadUnliftIO m)
  => Automation m
goldAutomation =
  Automation
    { _name = Gold
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
  info $ "Shutting down Gold"

  -- TODO FAIL APPROPRIATELY, LOG IT, AND STOP THREAD IF WE CAN'T LOAD THE DEVICE
  -- if gledoptoLedStrip = nullDevice then throwException and quit
  --  (_gledoptoLedStrip, ledTopic) <- Helpers.findDeviceM Device.GledoptoGLC007P_1

  info "turning led strip off"
  publishMQTT ledTopic "{\"state\": \"OFF\"}"

runAutomation
  :: (Logger m, MonadMQTT m, MonadReader (Env' logger mqttClient) m, MonadUnliftIO m)
  => TChan Message
  -> m ()
runAutomation broadcastChan = do
  info "Running Gold"

  -- TODO FAIL APPROPRIATELY, LOG IT, AND STOP THREAD IF WE CAN'T LOAD THE DEVICE
  -- (_gledoptoLedStrip, ledTopic) <- Helpers.findDeviceM Device.GledoptoGLC007P_1

  debug "turning on"
  publishMQTT ledTopic "{\"state\": \"ON\"}"

  debug "setting color to orange"
  publishMQTT ledTopic (hex' "be9fc1")

  liftIO $ threadDelay (seconds 2)

  debug "setting color to pink with 3 second transition"
  publishMQTT ledTopic (withTransition' 3 $ mkHex "F97C00")

  debug "starting breathe loop"
  go ledTopic broadcastChan

  where
    go
      :: (Logger m, MonadReader (Env' logger mqttClient) m, MonadMQTT m, MonadUnliftIO m)
      => Topic
      -> TChan Message
      -> m ()
    go ledTopic broadcastChan' = do
      liftIO $ threadDelay $ seconds 60
      debug "Gold: breathe"
      publishMQTT ledTopic $ effect' Breathe
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
            >> go ledTopic broadcastChan'
        _ -> go ledTopic broadcastChan'
