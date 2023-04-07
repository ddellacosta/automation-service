module Service.MQTT.Status
  ( encodeAutomationStatus
  )
where

import Control.Monad.IO.Unlift (MonadIO)
import Data.ByteString.Lazy (ByteString)
import Service.Env (Registrations, ThreadMap)
import Service.Device (DeviceId)
import Service.Group (GroupId)

encodeAutomationStatus :: (MonadIO m) => ThreadMap m -> Registrations DeviceId -> Registrations GroupId -> ByteString
encodeAutomationStatus running deviceRegs groupRegs = "{}"
