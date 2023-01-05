module Service.Actions.Chrizmaz
  ( chrizmazAction
  ,
  )
  where

import Control.Lens
import Control.Monad (forever)
import Control.Monad.IO.Unlift (liftIO, MonadUnliftIO)
import Data.Aeson (encode)
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Network.MQTT.Client as MQTT
import Service.ActionName (ActionName(..))
import Service.Action (Action(..), Message)
import Service.App (Logger(..))
import qualified Service.App.Helpers as Helpers
import Service.Messages.GledoptoGLC007P (Effect(..), mkEffectMsg, mkHex)
import qualified Service.Device as Device
import Service.Env (mqttClient)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM (TChan)

chrizmazAction :: UUID -> Action (TChan Message)
chrizmazAction newId = Action Chrizmaz newId [Device.GledoptoGLC007P_1] [Device.GledoptoGLC007P_1] initAction cleanupAction runAction 

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
runAction myName _broadcastChan = do
  info $ "Urnning Chrizmaz arbhft " <> myName <> " Jolly HO!"

  mc <- view mqttClient

-- christmas

  -- TODO FAIL APPROPRIATELY, LOG IT, AND STOP THREAD IF WE CAN'T LOAD THE DEVICE
  -- if gledoptoLedStrip = nullDevice then throwException and quit
  (_gledoptoLedStrip, ledTopic) <- Helpers.findDeviceM Device.GledoptoGLC007P_1

  info "turning on"
  liftIO $ MQTT.publish mc ledTopic "{\"state\": \"ON\"}" False

  forever $ do
    threadDelay (seconds 3)
    liftIO $ MQTT.publish mc ledTopic (hex' "009900") False
    liftIO $ MQTT.publish mc ledTopic (effect' Breathe) False
    threadDelay (seconds 3)
    liftIO $ MQTT.publish mc ledTopic (hex' "CC0000") False
    liftIO $ MQTT.publish mc ledTopic (effect' Breathe) False
-- end christmas

  where
    seconds n = n * 1000000
    effect' = encode . mkEffectMsg
    hex' = encode . mkHex
    -- withTransition' s msg = encode $ withTransition s msg
