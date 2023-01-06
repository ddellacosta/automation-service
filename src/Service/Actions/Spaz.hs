module Service.Actions.Spaz
  ( spazAction
  ,
  )
where

import Control.Lens
import Control.Monad (forever)
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
import Service.Messages.GledoptoGLC007P (Effect(..), mkColorXY, mkEffectMsg, mkHex, withTransition)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM (TChan, atomically, tryReadTChan)

spazAction :: UUID -> Action (TChan Message)
spazAction newId =
  Action Spaz newId [Device.GledoptoGLC007P_1] [Device.GledoptoGLC007P_1] initAction cleanupAction runAction

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

  liftIO $ MQTT.publish mc ledTopic (withTransition' 3 $ mkColorXY 0.7 0.28) False

  forever $ do
    threadDelay (10 `seconds`)
    liftIO $ MQTT.publish mc ledTopic (withTransition' 3 $ mkColorXY 0.1 0.12) False
    threadDelay (10 `seconds`)
    liftIO $ MQTT.publish mc ledTopic (withTransition' 3 $ mkColorXY 0.83 0.75) False
    threadDelay (10 `seconds`)
    liftIO $ MQTT.publish mc ledTopic (withTransition' 3 $ mkColorXY 0.7 0.28) False

--   forever $ do
--     threadDelay (seconds 3)
--     liftIO $ MQTT.publish mc ledTopic (hex' "009900") False
--     liftIO $ MQTT.publish mc ledTopic (effect' Breathe) False
--     threadDelay (seconds 3)
--     liftIO $ MQTT.publish mc ledTopic (hex' "CC0000") False
--     liftIO $ MQTT.publish mc ledTopic (effect' Breathe) False
-- end christmas

  where
    seconds n = n * 10000000
    effect' = encode . mkEffectMsg
    hex' = encode . mkHex
    mkColorXY' x y = encode $ mkColorXY x y
    withTransition' s msg = encode $ withTransition s msg
