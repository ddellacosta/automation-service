
module AutomationService.Device
  ( Device
  , DeviceId
  , Devices
  , Exposes
  , Feature
  , canGet
  , canSet
  , decodeDevice
  , isPublished
  )
where

import Prelude

import Data.Argonaut (Json, JsonDecodeError, decodeJson, toArray)
import Data.Argonaut.Decode.Combinators ((.:), (.:?))
import Data.Either (Either(..))
import Data.Int.Bits ((.&.))
import Data.Map (Map)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (for, traverse)

type DeviceId = String

isPublished :: Int -> Boolean
isPublished a = 1 .&. a > 0

canGet :: Int -> Boolean
canGet a = 2 .&. a > 0

canSet :: Int -> Boolean
canSet a = 4 .&. a > 0

type Feature =
  { fType :: String
  , name :: String
  , access :: Int
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
  access <- obj .: "access"
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
