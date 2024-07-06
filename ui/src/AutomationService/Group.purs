module Group where

import AutomationService.Device (DeviceId)
import Data.Argonaut (Json, JsonDecodeError(..), decodeJson, toArray)
import Data.Argonaut.Decode.Combinators ((.:))
import Data.Array (filter)
import Data.Either (Either(..), isRight)
import Data.Foldable (foldM)
import Data.Map as M
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Prelude (($), (<$>), (=<<), bind, pure)

type SceneId = Int
type SceneName = String
type DeviceEndpoint = Int

type Group =
  { name :: String
  , id :: Int
  , members :: Map DeviceId DeviceEndpoint
  , scenes :: Map SceneId SceneName
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
  members <- decodeMembers =<< obj .: "members"
  scenes <- decodeScenes =<< obj .: "scenes"
  pure $ { name, id, members, scenes }

  where
    decodeMembers
      :: Array Json
      -> Either JsonDecodeError (Map DeviceId DeviceEndpoint)
    decodeMembers = foldM decodeMember M.empty

    decodeMember
      :: Map DeviceId DeviceEndpoint
      -> Json
      -> Either JsonDecodeError (Map DeviceId DeviceEndpoint)
    decodeMember members memberJson = do
      obj <- decodeJson memberJson
      endpoint <- obj .: "endpoint"
      deviceId <- obj .: "ieee_address"
      pure $ M.insert deviceId endpoint members

    decodeScenes
      :: Array Json
      -> Either JsonDecodeError (Map SceneId SceneName)
    decodeScenes = foldM decodeScene M.empty

    decodeScene
      :: Map SceneId SceneName
      -> Json
      -> Either JsonDecodeError (Map SceneId SceneName)
    decodeScene scenes sceneJson = do
      obj <- decodeJson sceneJson
      sceneId <- obj .: "id"
      sceneName <- obj .: "name"
      pure $ M.insert sceneId sceneName scenes
