module AutomationService.Exposes
  ( BinaryProps
  , Exposes
  , CompositeProps
  , EnumProps
  , ListProps
  , NumericProps
  , SubProps(..)
  , ValueOnOff(..)
  , canGet
  , canSet
  , decodeExposes
  , isPublished
  , serializeValueOnOff
  )
where

import Control.Alternative ((<|>))
import Data.Argonaut (Json, JsonDecodeError, decodeJson, toArray)
import Data.Argonaut.Decode.Combinators ((.:), (.:?))
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Decoders (decodeBoolean, decodeString)
import Data.Int.Bits ((.&.))
import Data.Either (Either, fromRight, either)
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Data.Traversable (for, sequence)
import Prelude (class Eq, class Show, bind, const, discard, pure, show, (<<<), ($), (<$>), (=<<), (>), flip, (<>))
import Record (delete, merge)
import Type.Proxy (Proxy(..))

--
-- access https://www.zigbee2mqtt.io/guide/usage/exposes.html#access
--
isPublished :: Int -> Boolean
isPublished a = 1 .&. a > 0

canSet :: Int -> Boolean
canSet a = 2 .&. a > 0

-- this implies isPublished
canGet :: Int -> Boolean
canGet a = 4 .&. a > 0

-- https://www.zigbee2mqtt.io/guide/usage/exposes.html#binary

data ValueOnOff = ValueOnOffBool Boolean | ValueOnOffString String

derive instance Generic ValueOnOff _
derive instance Eq ValueOnOff

instance DecodeJson ValueOnOff where
  decodeJson :: Json -> Either JsonDecodeError ValueOnOff
  decodeJson json =
    ValueOnOffBool <$> decodeBoolean json <|> ValueOnOffString <$> decodeString json

instance Show ValueOnOff where
  show = genericShow

serializeValueOnOff :: ValueOnOff -> String
serializeValueOnOff = case _ of
  ValueOnOffBool b -> show b
  ValueOnOffString s -> s

type BinaryProps =
  { valueOn     :: ValueOnOff
  , valueOff    :: ValueOnOff
  , valueToggle :: Maybe String
  } 

type CompositeProps = Array Exposes

type EnumProps = { values :: Array String }

type ListProps = Exposes

type NumericProps =
  { valueMax  :: Maybe Int
  , valueMin  :: Maybe Int
  , valueStep :: Maybe Int
  , unit      :: Maybe String
  }

data SubProps
  = Binary BinaryProps
  | Composite CompositeProps
  | Enum EnumProps
  | List ListProps
  | Null
  | Numeric NumericProps

derive instance Generic SubProps _

instance Show SubProps where
  -- apparently due to the fact that SubProps can be recursive
  -- via ListProps or CompositeProps, point-free here introduces a
  -- cycle that won't type-check, I don't really understand why yet
  -- though (TODO)
  show sp = genericShow sp

type Exposes =
  { type        :: String
  , featureType :: Maybe String
  , name        :: String
  , description :: Maybe String
  , label       :: String
  -- Maybe only because it doesn't exist inside list type 🤮
  , property    :: Maybe String
  , access      :: Int
  , subProps    :: SubProps
  }

decodeBaseExposes
  :: Maybe String -> Json -> Either JsonDecodeError Exposes
decodeBaseExposes featureType exposesJson = do
  obj <- decodeJson exposesJson
  type' <- obj .: "type"
  name <- obj .: "name"
  description <- obj .:? "description"
  label <- obj .: "label"
  property <- obj .:? "property"
  access <- obj .: "access"
  pure { type: type', featureType, name, description, label, property, access, subProps: Null }

decodeBinary :: Json -> Either JsonDecodeError BinaryProps
decodeBinary exposesJson = do
  obj <- decodeJson exposesJson
  valueOn <- obj .: "value_on"
  valueOff <- obj .: "value_off"
  valueToggle <- obj .:? "value_toggle"
  pure { valueOn, valueOff, valueToggle }

decodeEnum :: Json -> Either JsonDecodeError EnumProps
decodeEnum exposesJson = do
  obj <- decodeJson exposesJson
  values <- obj .: "values"
  pure { values }

decodeNumeric :: Json -> Either JsonDecodeError NumericProps
decodeNumeric exposesJson = do
  obj <- decodeJson exposesJson
  valueMax <- obj .:? "value_max"
  valueMin <- obj .:? "value_min"
  valueStep <- obj .:? "value_step"
  unit <- obj .:? "unit"
  pure { valueMax, valueMin, valueStep, unit }

decodeExposes :: Maybe String -> Array Json -> Either JsonDecodeError (Array Exposes)
decodeExposes featureType exposesJson = do
  --
  -- This flattens out the features array into the same array that
  -- individual 'exposes' entries are returned in. Zigbee2MQTT
  -- distinguishes these as 'specific' (the former) vs. 'generic' (the
  -- latter). However, I don't see much value in separating them like
  -- this given how we'll be looking this data up.
  --
  -- https://www.zigbee2mqtt.io/guide/usage/exposes.html
  --
  fold <$>
    --
    -- for
    --   :: Array exposesJson
    --   -> (exposesJson -> Either JsonDecodeError (Array Exposes))
    --   -> Either JsonDecodeError (Array (Array Exposes))
    --
    (for exposesJson $ \e -> do
        obj <- decodeJson e
        features <- obj .:? "features"
        featureType' <- obj .:? "type"
        case features of
          Just features' -> for features' $ decodeExposes' featureType'
          _ -> for [e] $ decodeExposes' featureType
    )

  where
    decodeExposes' :: Maybe String -> Json -> Either JsonDecodeError Exposes
    decodeExposes' featureType exposesJson = do
      obj <- decodeJson exposesJson
      exposed <- decodeBaseExposes featureType exposesJson
      mFeatures <- obj .:? "features"
      mItemType <- obj .:? "item_type"

      case exposed.type, mFeatures, mItemType of
        "binary", _, _ -> do
          binary <- Binary <$> decodeBinary exposesJson
          pure $ exposed { subProps = binary }
        "enum", _, _ -> do
          enum <- Enum <$> decodeEnum exposesJson
          pure $ exposed { subProps = enum }
        "numeric", _, _ -> do
          numeric <- Numeric <$> decodeNumeric exposesJson
          pure $ exposed { subProps = numeric }
        "composite", Just features, _ -> do
          composite <- Composite <$> (for features $ decodeExposes' Nothing)
          pure $ exposed { subProps = composite }
        "list", _, Just itemType -> do
          list <- List <$> decodeExposes' Nothing itemType
          pure $ exposed { subProps = list }
        _, _, _ -> pure exposed -- subProps = Null per decodeBaseExposes
