module Service.Actions.Chrizmaz
  ( chrizmazAction
  ,
  )
  where

import Control.Monad (forever)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Text (Text)
import Data.UUID (UUID)
import Service.ActionName (ActionName(..))
import Service.Action (Action, ActionFor(..), Message)
import Service.App (Logger(..), MonadMQTT(..))
import qualified Service.App.Helpers as Helpers
import Service.Messages.GledoptoGLC007P (Effect(..), effect', hex', seconds)
import qualified Service.Device as Device
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM (TChan)

chrizmazAction :: (Logger m, MonadMQTT m, MonadUnliftIO m) => UUID -> Action m
chrizmazAction newId = ActionFor Chrizmaz newId [Device.GledoptoGLC007P_1] [Device.GledoptoGLC007P_1] initAction cleanupAction runAction 

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
  info $ "Urnning Chrizmaz arbhft " <> myName <> " Jolly HO!"

  -- TODO FAIL APPROPRIATELY, LOG IT, AND STOP THREAD IF WE CAN'T LOAD THE DEVICE
  -- if gledoptoLedStrip = nullDevice then throwException and quit
  (_gledoptoLedStrip, ledTopic) <- Helpers.findDeviceM Device.GledoptoGLC007P_1

  debug "turning on"
  publishMQTT ledTopic "{\"state\": \"ON\"}"

  forever $ do
    threadDelay (seconds 3)
    publishMQTT ledTopic (hex' "009900")
    publishMQTT ledTopic (effect' Breathe)
    threadDelay (seconds 3)
    publishMQTT ledTopic (hex' "CC0000")
    publishMQTT ledTopic (effect' Breathe)
