module Service.App.Helpers
  ( findDeviceM
  )
  where

import Control.Lens ((^.), view)
import Control.Monad.Reader (MonadReader)
import qualified Network.MQTT.Client as MQTT
import Service.Device (Device, DeviceId, findDevice, topic)
import Service.Env (Env', config, devices)

findDeviceM
  :: (MonadReader (Env' logger mqttClient) m)
  => DeviceId
  -> m (Device, MQTT.Topic)
findDeviceM deviceId = do
  devices' <- view (config . devices)
  let device = findDevice deviceId devices'
  pure (device, device ^. topic)
