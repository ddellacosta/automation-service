module Service.App.Helpers
  ( findDeviceM
  )
  where

import Control.Lens (view)
import Control.Monad.Reader (MonadReader)
import Control.Monad.IO.Unlift (MonadIO)
import qualified Data.Map.Strict as M
import Service.Device (Device, DeviceId)
import Service.Env (Env, devices)
import UnliftIO.STM (atomically, readTVar)

findDeviceM
  :: (MonadIO m, MonadReader Env m)
  => DeviceId
  -> m (Maybe Device)
findDeviceM deviceId = do
  storedDevices <- view devices
  storedDevices' <- atomically $ readTVar storedDevices
  pure $ M.lookup deviceId storedDevices'
