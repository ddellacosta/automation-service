module Service.Actions.Gold
  ( goldAction
  ,
  )
where

import Prelude hiding (id, init)

import Control.Monad.Reader (MonadReader, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Text (Text)
import Network.MQTT.Client (Topic)
import Service.App (Logger(..), MonadMQTT(..))
import qualified Service.App.Helpers as Helpers
import Service.Action (Action, ActionFor(..), Message(..), MsgBody(..))
import Service.ActionName (ActionName(..))
import qualified Service.Device as Device
import Service.Env (Env')
import Service.Messages.GledoptoGLC007P
  ( Effect(..)
  , effect'
  , hex'
  , mkHex
  , seconds
  , withTransition'
  )
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM (TChan, atomically, tryReadTChan)

goldAction :: (Logger m, MonadMQTT m, MonadReader (Env' logger mqttClient) m, MonadUnliftIO m) => Action m
goldAction =
  ActionFor
    { name = Gold
    , devices = [Device.GledoptoGLC007P_1]
    , wantsFullControlOver = [Device.GledoptoGLC007P_1]
    , cleanup = cleanupAction
    , run = runAction
    }

cleanupAction
  :: (Logger m, MonadMQTT m, MonadReader (Env' logger mqttClient) m, MonadUnliftIO m)
  => TChan Message
  -> m ()
cleanupAction _broadcastChan = do
  info $ "Shutting down Gold"

  -- TODO FAIL APPROPRIATELY, LOG IT, AND STOP THREAD IF WE CAN'T LOAD THE DEVICE
  -- if gledoptoLedStrip = nullDevice then throwException and quit
  (_gledoptoLedStrip, ledTopic) <- Helpers.findDeviceM Device.GledoptoGLC007P_1

  info "turning led strip off"
  publishMQTT ledTopic "{\"state\": \"OFF\"}"

runAction
  :: (Logger m, MonadMQTT m, MonadReader (Env' logger mqttClient) m, MonadUnliftIO m)
  => TChan Message
  -> m ()
runAction broadcastChan = do
  info "Running Gold"

  -- TODO FAIL APPROPRIATELY, LOG IT, AND STOP THREAD IF WE CAN'T LOAD THE DEVICE
  (_gledoptoLedStrip, ledTopic) <- Helpers.findDeviceM Device.GledoptoGLC007P_1

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
      liftIO $ threadDelay (seconds 60)
      debug "Gold: breathe"
      publishMQTT ledTopic (effect' Breathe)

      -- this bit is just a proto-PoC right now, doesn't really do anything
      maybeMsg <- atomically $ tryReadTChan broadcastChan'
      case maybeMsg of
        Just (Client (MsgBody msg')) ->
          debug ("Gold -- msg: " <> msg') >> go ledTopic broadcastChan'
        _ -> go ledTopic broadcastChan'
