module Test.Helpers
  ( loadOneDay
  , loadTestDevices
  , loadTestGroups
  , 
  )
where

import Data.Aeson (Value, decode)
import Data.Aeson.Types (emptyObject)
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

loadOneDay :: IO Value
loadOneDay = do
  oneDayRawJSON <- BL.fromStrict <$> BS.readFile "test/fixtures/oneday.json"
  pure $ fromMaybe emptyObject $ decode oneDayRawJSON
