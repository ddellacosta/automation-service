module Service.Automations.Gold
  ( goldAutomation
  ,
  )
where

import Prelude hiding (id, init)

import Control.Lens (view, (^.), (^?))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader, liftIO)
import Data.Aeson.Lens (key)
import Data.Foldable (for_)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Network.MQTT.Client (Topic)
import Service.App (Logger (..), debug, findDeviceM, info, publish)
import qualified Service.Automation as Automation
import Service.Automation (Automation (..))
import Service.AutomationName (AutomationName (..))
import Service.Device (DeviceId, topicSet)
import Service.Env (Env, daemonBroadcast)
import Service.Group (GroupId)
import Service.MQTT.Class (MQTTClient (..))
import qualified Service.MQTT.Messages.Daemon as Daemon
import Service.MQTT.Messages.Lighting (Effect (..), effect', hex', mkHex, seconds, withTransition')
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM (TChan, atomically, tryReadTChan, writeTChan)

mirrorLightId :: DeviceId
mirrorLightId = "0xb4e3f9fffe14c707"

basementStandingLampGroupId :: GroupId
basementStandingLampGroupId = 1

goldAutomation
  :: (Logger l, MQTTClient mc, MonadReader (Env l mc) m, MonadUnliftIO m)
  => UTCTime
  -> Automation m
goldAutomation ts =
  Automation
    { _name = Gold
    , _cleanup = cleanupAutomation
    , _run = runAutomation
    , _startTime = ts
    }

cleanupAutomation
  :: (Logger l, MQTTClient mc, MonadReader (Env l mc) m, MonadUnliftIO m)
  => TChan Automation.Message
  -> m ()
cleanupAutomation _broadcastChan = do
  info $ "Shutting down Gold"

  lightStrip <- findDeviceM mirrorLightId

  for_ lightStrip $ \lightStrip' -> do
    let lightStripTopic = lightStrip' ^. topicSet

    info "turning led strip off"
    publish lightStripTopic "{\"state\": \"OFF\"}"

runAutomation
  :: (Logger l, MQTTClient mc, MonadReader (Env l mc) m, MonadUnliftIO m)
  => TChan Automation.Message
  -> m ()
runAutomation broadcastChan = do
  info "Running Gold"

  daemonBroadcast' <- view daemonBroadcast

  let
    mirrorLightRegMsg = Daemon.RegisterDevice mirrorLightId Gold
    basementStandingLampRegMsg = Daemon.RegisterGroup basementStandingLampGroupId Gold

  atomically $ writeTChan daemonBroadcast' mirrorLightRegMsg
  atomically $ writeTChan daemonBroadcast' basementStandingLampRegMsg

  -- note that this is going to return Nothing in Integration tests
  -- without explicitly loading the group and device having the IDs
  -- above
  lightStrip <- findDeviceM mirrorLightId

  for_ lightStrip $ \lightStrip' -> do
    let lightStripTopic = lightStrip' ^. topicSet

    debug "turning on"
    publish lightStripTopic "{\"state\": \"ON\"}"

    debug "setting color to orange"
    publish lightStripTopic (hex' "be9fc1")

    liftIO $ threadDelay (seconds 2)

    debug "setting color to pink with 3 second transition"
    publish lightStripTopic (withTransition' 3 $ mkHex "F97C00")

    debug "starting breathe loop"
    go lightStripTopic broadcastChan

  where
    go
      :: (Logger l, MonadReader (Env l mc) m, MQTTClient mc, MonadUnliftIO m)
      => Topic
      -> TChan Automation.Message
      -> m ()
    go lightStripTopic broadcastChan' = do
      liftIO $ threadDelay $ seconds 60
      debug "Gold: breathe"
      publish lightStripTopic $ effect' Breathe
      --
      -- For now, this and GoldMsg below are just here to remind me
      -- how to create and use per-Automation message types...actually,
      -- testing it just now this doesn't seem to pick up messages
      -- anyways. TODO: fix
      --
      maybeMsg <- atomically $ tryReadTChan broadcastChan'
      debug $ "Gold got msg? " <> T.pack (show maybeMsg)
      case maybeMsg of
        Just (Automation.Client Gold (Automation.ValueMsg msg')) ->
          debug
            ( T.pack $
                "Gold received msg. Mood is " <>
                show (msg' ^? key "mood") <>
                ", Fancy = " <>
                show (msg' ^? key "fancy")
            )
            >> go lightStripTopic broadcastChan'
        _ -> go lightStripTopic broadcastChan'
