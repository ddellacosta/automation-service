module Test.Helpers
  ( devicesRawJSON
  , groupsRawJSON
  , loadTestDevices
  , loadTestGroups
  ,
  )
where

import Data.Aeson (decode)
import Data.ByteString as BS
import Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Service.Device (Device)
import Service.Group (Group)

devicesRawJSON :: IO BL.ByteString
devicesRawJSON = BL.fromStrict <$> BS.readFile "test/fixtures/devices.json"

loadTestDevices :: IO [Device]
loadTestDevices = pure . fromMaybe [] . decode =<< devicesRawJSON

groupsRawJSON :: IO BL.ByteString
groupsRawJSON = BL.fromStrict <$> BS.readFile "test/fixtures/groups.json"

loadTestGroups :: IO [Group]
loadTestGroups = pure . fromMaybe [] . decode =<< groupsRawJSON
