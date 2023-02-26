module Service.App.Helpers
  ( findDeviceM
  , findDeviceTopicM
  )
  where

import Control.Lens ((^.), view)
import Control.Monad.Reader (MonadReader)
import Control.Monad.IO.Unlift (MonadIO)
import Data.Maybe (fromMaybe)
import qualified Network.MQTT.Client as MQTT
import Safe (headMay)
import Service.Device (Device(_id), DeviceId)
import Service.Env (Env, config, devices)
import Service.Messages.Zigbee2MQTTDevice as Zigbee2MQTT
import UnliftIO.STM (atomically, readTVar)

findDeviceM
  :: (MonadIO m, MonadReader Env m)
  => DeviceId
  -> m (Maybe Device)
findDeviceM deviceId = do
  storedDevices <- view devices
  devices' <- atomically $ readTVar storedDevices
  pure . headMay . filter ((== deviceId) . _id) $ devices'

findDeviceTopicM
  :: (MonadIO m, MonadReader Env m) => DeviceId -> m MQTT.Topic
findDeviceTopicM deviceId = do
  device <- findDeviceM deviceId
  pure $ fromMaybe "" $ Zigbee2MQTT.deviceSetterTopic =<< device
