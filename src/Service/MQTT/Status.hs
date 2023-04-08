module Service.MQTT.Status
  ( encodeAutomationStatus
  )
where

import qualified Data.Aeson as Aeson
import Data.Aeson (encode, object, toJSON)
import Data.Aeson.Types (emptyArray)
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as M
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Vector as V
import Service.AutomationName (serializeAutomationName)
import Service.Env (Registrations, ScheduledJobs, ThreadMap, invertRegistrations)
import Service.Device (DeviceId)
import Service.Group (GroupId)

encodeAutomationStatus
  :: (Applicative m)
  => ThreadMap m
  -> ScheduledJobs
  -> Registrations DeviceId
  -> Registrations GroupId
  -> ByteString
encodeAutomationStatus running scheduled deviceRegs groupRegs = encode $
  object
    [ ("runningAutomations", running')
    , ("scheduledAutomations", scheduled')
    ]

  where
    deviceRegs' = invertRegistrations deviceRegs
    groupRegs' = invertRegistrations groupRegs

    deviceArray autoName =
      maybe
        emptyArray
        (Aeson.Array . V.fromList . (fmap $ Aeson.String) . NE.toList) .
      M.lookup autoName

    groupArray autoName =
      maybe
        emptyArray
        (Aeson.Array . V.fromList . (fmap toJSON) . NE.toList) .
      M.lookup autoName

    running' = Aeson.Array $ V.fromList $ flip M.foldMapWithKey running $ \autoName _v ->
      [ object
        [ ("name", Aeson.String $ serializeAutomationName autoName)
        , ("devices", deviceArray autoName deviceRegs')
        , ("groups", groupArray autoName groupRegs')
        ]
      ]

    scheduled' = Aeson.Array $ V.fromList $ flip M.foldMapWithKey scheduled $
      \k (_, msg, _) ->
        [ object
          [("jobId", Aeson.String k), ("job", Aeson.String $ T.pack $ show msg)]
        ]
