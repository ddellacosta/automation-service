module Service.App.Helpers
  ( findDeviceM
  , findThreadsByDeviceId
  )
  where

import Control.Lens ((<&>), view, (^.))
import Control.Monad.Reader (MonadReader)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Network.MQTT.Client as MQTT
import Service.Action (Action)
import Service.App.DaemonState (DeviceMap, ThreadMap)
import Service.Device (Device, DeviceId, findDevice, topic)
import Service.Env (Env', config, devices)
import UnliftIO.Async (Async)

findDeviceM
  :: (MonadReader (Env' logger mqttClient) m)
  => DeviceId
  -> m (Device, MQTT.Topic)
findDeviceM deviceId = do
  devices' <- view (config . devices)
  let device = findDevice deviceId devices'
  pure (device, device ^. topic)

findThreadsByDeviceId :: [DeviceId] -> ThreadMap m -> DeviceMap -> [(Action m, Async ())]
findThreadsByDeviceId devices' threadMap' deviceMap' = mconcat $ devices' <&> \did ->
  case M.lookup did deviceMap' of
    Just actionNames ->
      mconcat . catMaybes $ actionNames <&> flip M.lookup threadMap'
    Nothing -> []
