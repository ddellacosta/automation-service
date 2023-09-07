
module AutomationService.Device
  ( Access
  , Device
  , DeviceId
  , Devices
  , Exposes
  , Feature
  , decodeDevice
  )
where

import Prelude

import Control.Alternative (guard)
import Data.Argonaut (Json, JsonDecodeError, decodeJson, toArray)
import Data.Argonaut.Decode.Combinators ((.:), (.:?))
import Data.Array (catMaybes)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int.Bits ((.&.))
import Data.Map (Map)
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Data.Traversable (for, traverse)

type DeviceId = String

type Access =
  { published :: Boolean
  , get :: Boolean
  , set :: Boolean
  }

decodeAccess :: Int -> Access
decodeAccess accessInt =
  { published: 1 .&. accessInt > 0
  , get:       2 .&. accessInt > 0
  , set:       4 .&. accessInt > 0
  }

type Feature =
  { fType :: String
  , name :: String
  , access :: Access
  }

type Exposes = Array Feature


-- todo: first check to see if we have a `features` key or not...then 
-- we'll have to do that recursively for other sub-objects. Can we 
-- represent this with self-referential data somehow?
decodeExposes :: Array Json -> Either JsonDecodeError Exposes
decodeExposes = traverse \exposed -> do
  obj <- decodeJson exposed
  fType <- obj .: "type"
  name <- obj .: "name"
  accessInt <- obj .: "access"
  access <- Right $ decodeAccess accessInt
  pure { fType, name, access }

type Device =
  { id :: DeviceId
  , name :: String
  , category :: String
  , manufacturer :: Maybe String
  , model :: Maybe String
  , exposes :: Maybe Exposes
  }

decodeDevice :: Json -> Either JsonDecodeError Device
decodeDevice json = do
  obj <- decodeJson json
  id <- obj .: "ieee_address"
  name <- obj .: "friendly_name"
  category <- obj .: "type"
  manufacturer <- obj .:? "manufacturer"
  model <- obj .:? "model_id"
  definition <- obj .:? "definition"
  exposesRaw <- maybe (Right Nothing) (flip (.:?) "exposes") definition
  exposes <- for (exposesRaw >>= toArray) decodeExposes
  pure { id, name, category, manufacturer, model, exposes }

type Devices = Map DeviceId Device
