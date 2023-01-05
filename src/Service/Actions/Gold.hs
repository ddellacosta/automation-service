module Service.Actions.Gold
  ( goldAction
  ,
  )
where

import Control.Lens
import Control.Monad.Reader (MonadReader, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Aeson (encode)
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Network.MQTT.Client as MQTT
import Service.App (Logger(..))
import qualified Service.App.Helpers as Helpers
import Service.Action (Action(..), Message(..), MsgBody(..))
import Service.ActionName (ActionName(..))
import qualified Service.Device as Device
import Service.Env (Env, mqttClient)
import Service.Messages.GledoptoGLC007P (Effect(..), mkEffectMsg, mkHex, withTransition)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM (TChan, atomically, tryReadTChan)

goldAction :: UUID -> Action (TChan Message)
goldAction newId =
  Action Gold newId [Device.GledoptoGLC007P_1] [Device.GledoptoGLC007P_1] initAction cleanupAction runAction

initAction :: (MonadUnliftIO m) => Text -> TChan Message -> m (TChan Message)
initAction _myName = pure

cleanupAction :: (Logger m, MonadUnliftIO m) => Text -> TChan Message -> m ()
cleanupAction myName _broadcastChan = do
  info $ "Shutting down " <> myName

  mc <- view mqttClient

  -- TODO FAIL APPROPRIATELY, LOG IT, AND STOP THREAD IF WE CAN'T LOAD THE DEVICE
  -- if gledoptoLedStrip = nullDevice then throwException and quit
  (_gledoptoLedStrip, ledTopic) <- Helpers.findDeviceM Device.GledoptoGLC007P_1

  info "turning led strip off"
  liftIO $ MQTT.publish mc ledTopic "{\"state\": \"OFF\"}" False

runAction :: (Logger m, MonadUnliftIO m) => Text -> TChan Message -> m ()
runAction myName broadcastChan = do
  info $ "Running " <> myName

  mc <- view mqttClient

  -- TODO FAIL APPROPRIATELY, LOG IT, AND STOP THREAD IF WE CAN'T LOAD THE DEVICE
  (_gledoptoLedStrip, ledTopic) <- Helpers.findDeviceM Device.GledoptoGLC007P_1

  info "turning on"
  liftIO $ MQTT.publish mc ledTopic "{\"state\": \"ON\"}" False

  info "setting color to orange"
  liftIO $ MQTT.publish mc ledTopic (hex' "be9fc1") False
  liftIO $ threadDelay (seconds 2)

  info "setting color to pink with 3 second transition"
  liftIO $ MQTT.publish mc ledTopic (withTransition' 3 $ mkHex "F97C00") False

  info "starting breathe loop"
  go ledTopic broadcastChan

  where
    go :: (Logger m, MonadReader Env m, MonadUnliftIO m) => MQTT.Topic -> TChan Message -> m ()
    go ledTopic broadcastChan' = do
      mc <- view mqttClient
      liftIO $ threadDelay (seconds 60)
      info $ myName <> ": breathe"
      liftIO $ MQTT.publish mc ledTopic (effect' Breathe) False

      maybeMsg <- atomically $ tryReadTChan broadcastChan'
      case maybeMsg of
        Just (Client (MsgBody msg')) ->
          (info $ myName <> ", msg: " <> msg') >> go ledTopic broadcastChan'
        _ -> go ledTopic broadcastChan'

    seconds n = n * 1000000
    effect' = encode . mkEffectMsg
    hex' = encode . mkHex
    withTransition' s msg = encode $ withTransition s msg

