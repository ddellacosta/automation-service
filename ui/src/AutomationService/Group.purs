module AutomationService.Group
  ( Group
  , GroupDevice
  , GroupScene
  , decodeGroups
  )
where

import AutomationService.Device (Device, Devices)
import Data.Argonaut (Json, JsonDecodeError(..), decodeJson, toArray)
import Data.Argonaut.Decode.Combinators ((.:))
import Data.Array (filter)
import Data.Either (Either(..), isRight)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Prelude ((<<<), ($), (<$>), (=<<), (<>), bind, pure)

type GroupScene = { id :: Int, name :: String }
type GroupDevice = { id :: String, endpoint :: Int, device :: Device }

type Group =
  { name :: String
  , id :: Int
  , members :: Array GroupDevice
  , scenes :: Array GroupScene
  }

decodeGroups :: Devices -> Json -> Either JsonDecodeError (Array Group)
decodeGroups devices groupsJson = do
  case toArray groupsJson of
    Just gs -> 
      -- REPORT THESE FAILURES! Either isn't really enough for what I want here?
      case (filter isRight $ decodeGroup devices <$> gs) of
        [] -> Left (UnexpectedValue groupsJson)
        ds' -> sequence ds'
    Nothing ->
      Left (UnexpectedValue groupsJson)

decodeGroup :: Devices -> Json -> Either JsonDecodeError Group
decodeGroup devices groupJson = do
  obj <- decodeJson groupJson
  name <- obj .: "friendly_name"
  id <- obj .: "id"
  members <- decodeGroupResource (decodeMember devices) =<< obj .: "members"
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

    decodeMember :: Devices -> Json -> Either JsonDecodeError GroupDevice
    decodeMember devices' memberJson = do
      obj <- decodeJson memberJson
      endpoint <- obj .: "endpoint"
      id <- obj .: "ieee_address"
      case M.lookup id devices' of
        Just device ->
          pure { id, endpoint, device }
        Nothing ->
          Left <<< TypeMismatch $ "No device with id " <> id <> " exists"

    decodeScene :: Json -> Either JsonDecodeError GroupScene
    decodeScene sceneJson = do
      obj <- decodeJson sceneJson
      id <- obj .: "id"
      name <- obj .: "name"
      pure { id, name }
