module Service.Actions.Trinity
  ( trinityAction
  ,
  )
where

import Control.Monad (forever)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Text (Text)
import Data.UUID (UUID)
import Service.App (Logger(..), MonadMQTT(..))
import qualified Service.App.Helpers as Helpers
import Service.Action (Action, ActionFor(..), Message(..))
import Service.ActionName (ActionName(..))
import qualified Service.Device as Device
import Service.Messages.GledoptoGLC007P (mkColorXY, seconds, withTransition')
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM (TChan)

trinityAction :: (Logger m, MonadMQTT m, MonadUnliftIO m) => UUID -> Action m
trinityAction newId =
  ActionFor Trinity newId [Device.GledoptoGLC007P_1] [Device.GledoptoGLC007P_1] initAction cleanupAction runAction

initAction :: (MonadUnliftIO m) => Text -> TChan Message -> m (TChan Message)
initAction _myName = pure

cleanupAction :: (Logger m, MonadMQTT m, MonadUnliftIO m) => Text -> TChan Message -> m ()
cleanupAction myName _broadcastChan = do
  info $ "Shutting down " <> myName

  -- TODO FAIL APPROPRIATELY, LOG IT, AND STOP THREAD IF WE CAN'T LOAD THE DEVICE
  -- if gledoptoLedStrip = nullDevice then throwException and quit
  (_gledoptoLedStrip, ledTopic) <- Helpers.findDeviceM Device.GledoptoGLC007P_1

  info "turning led strip off"
  publishMQTT ledTopic "{\"state\": \"OFF\"}"

runAction :: (Logger m, MonadMQTT m, MonadUnliftIO m) => Text -> TChan Message -> m ()
runAction myName _broadcastChan = do
  info $ "Running " <> myName

  -- TODO FAIL APPROPRIATELY, LOG IT, AND STOP THREAD IF WE CAN'T LOAD THE DEVICE
  (_gledoptoLedStrip, ledTopic) <- Helpers.findDeviceM Device.GledoptoGLC007P_1

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
