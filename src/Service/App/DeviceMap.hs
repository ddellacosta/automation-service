module Service.App.DeviceMap
  ( DeviceMap
  , insertDeviceActions
  )
  where

import qualified Data.Map.Strict as M
import Service.ActionName (ActionName)
import Service.Device (DeviceId)
import UnliftIO.STM (STM, TVar, readTVar, writeTVar)

type DeviceMap = M.Map DeviceId [ActionName]

-- |
-- | Given a TVar DeviceMap, this will add a new ActionName entry for
-- | all matching DeviceIds passed in.
-- |
insertDeviceActions :: TVar DeviceMap -> [DeviceId] -> ActionName -> STM ()
insertDeviceActions deviceMap devices actionName = do
  deviceMap' <- readTVar deviceMap
  mapM_ (\deviceId ->
    writeTVar deviceMap $
      M.alter
        (\case
            Nothing -> Just [actionName]
            Just actionNames -> Just (actionNames <> [actionName])
        )
        deviceId
        deviceMap'
    )
    devices
