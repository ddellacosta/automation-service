module Test.Helpers
  ( loadTestDevices
  , loadTestGroups
  , 
  )
where

import Data.Aeson (decode)
import Data.ByteString.Lazy as BL
import Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Service.Device (Device)
import Service.Group (Group)

loadTestDevices :: IO [Device]
loadTestDevices = do
  devicesRawJSON <- BL.fromStrict <$> BS.readFile "test/fixtures/devices.json"
  pure $ fromMaybe [] $ decode devicesRawJSON

loadTestGroups :: IO [Group]
loadTestGroups = do
  groupsRawJSON <- BL.fromStrict <$> BS.readFile "test/fixtures/groups.json"
  pure $ fromMaybe [] $ decode groupsRawJSON
