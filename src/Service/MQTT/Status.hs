module Service.MQTT.Status
  ( encodeAutomationStatus
  )
where

import Control.Lens ((^.))
import qualified Data.Aeson as Aeson
import Data.Aeson (encode, object, toJSON)
import Data.Aeson.Types (emptyArray)
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as M
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Data.Vector as V
import Service.Automation (startTime)
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

    deviceRegs' autoName = deviceArray autoName . invertRegistrations $ deviceRegs
    groupRegs' autoName = groupArray autoName . invertRegistrations $ groupRegs

    startTime' = Aeson.String . T.pack . iso8601Show

    running' = Aeson.Array $ V.fromList $ flip M.foldMapWithKey running $ \autoName (auto, _async) ->
      [ object
        [ ("name", Aeson.String $ serializeAutomationName autoName)
        , ("devices", deviceRegs' autoName)
        , ("groups", groupRegs' autoName)
        , ("startTime", startTime' (auto ^. startTime))
        ]
      ]

    scheduled' = Aeson.Array $ V.fromList $ flip M.foldMapWithKey scheduled $
      \k (_, msg, _) ->
        [ object
          [("jobId", Aeson.String k), ("job", Aeson.String $ T.pack $ show msg)]
        ]
