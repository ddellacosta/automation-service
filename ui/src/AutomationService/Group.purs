module AutomationService.Group
  ( Group
  , GroupDevice
  , GroupScene
  , decodeGroups
  )
where

import AutomationService.Device (DeviceId)
import Data.Argonaut (Json, JsonDecodeError(..), decodeJson, encodeJson, toArray)
import Data.Argonaut.Decode.Combinators ((.:))
import Data.Array (filter)
import Data.Either (Either(..), isRight)
import Data.Foldable (foldM)
import Data.Map as M
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse)
import Prelude (($), (<$>), (=<<), (<#>), bind, map, pure)

type GroupScene = { id :: Int, name :: String }
type GroupDevice = { id :: String, endpoint :: Int }

type Group =
  { name :: String
  , id :: Int
  , members :: Array GroupDevice
  , scenes :: Array GroupScene
  }

decodeGroups :: Json -> Either JsonDecodeError (Array Group)
decodeGroups groupsJson = do
  case toArray groupsJson of
    Just gs -> 
      -- REPORT THESE FAILURES! Either isn't really enough for what I want here?
      case (filter isRight $ decodeGroup <$> gs) of
        [] -> Left (UnexpectedValue groupsJson)
        ds' -> sequence ds'
    Nothing ->
      Left (UnexpectedValue groupsJson)

decodeGroup :: Json -> Either JsonDecodeError Group
decodeGroup groupJson = do
  obj <- decodeJson groupJson
  name <- obj .: "friendly_name"
  id <- obj .: "id"
  members <- decodeGroupResource decodeMember =<< obj .: "members"
  scenes <- decodeGroupResource decodeScene =<< obj .: "scenes"
  pure { name, id, members, scenes }

  where
    decodeGroupResource
      :: forall a. (Json -> Either JsonDecodeError a)
      -> Array Json
      -> Either JsonDecodeError (Array a)
    decodeGroupResource decodeFn grs = case (filter isRight $ decodeFn <$> grs) of
      [] -> Right []
      grs' -> sequence grs'

    decodeMember :: Json -> Either JsonDecodeError GroupDevice
    decodeMember memberJson = do
      obj <- decodeJson memberJson
      endpoint <- obj .: "endpoint"
      id <- obj .: "ieee_address"
      pure { id, endpoint }

    decodeScene :: Json -> Either JsonDecodeError GroupScene
    decodeScene sceneJson = do
      obj <- decodeJson sceneJson
      id <- obj .: "id"
      name <- obj .: "name"
      pure { id, name }
