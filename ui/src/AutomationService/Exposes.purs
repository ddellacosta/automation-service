module AutomationService.Exposes
  ( BinaryProps
  , Exposes
  , CompositeProps
  , EnumProps
  , ListProps
  , NullProps
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

import Debug (trace, traceM)

import Prelude (class Eq, class Show, bind, const, discard, pure, show, (<<<), ($), (<$>), (=<<), (>), flip, (<>))

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

type EnumProps = { values :: Array String }

type NumericProps =
  { valueMax  :: Maybe Int
  , valueMin  :: Maybe Int
  , valueStep :: Maybe Int
  , unit      :: Maybe String
  }

type NullProps = Record ()

data SubProps
  = Binary BinaryProps
  | Composite CompositeProps
  | Enum EnumProps
  | List ListProps
  | Null NullProps
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

type CompositeProps = Array Exposes

type ListProps = { itemType :: Exposes }

decodeBaseExposes
  :: Maybe String -> Json -> Either JsonDecodeError Exposes
decodeBaseExposes featureType capabilityJson = do
  obj <- decodeJson capabilityJson
  type' <- obj .: "type"
  name <- obj .: "name"
  description <- obj .:? "description"
  label <- obj .: "label"
  property <- obj .:? "property"
  access <- obj .: "access"
  pure { type: type', featureType, name, description, label, property, access, subProps: Null {} }

decodeBinary :: Json -> Either JsonDecodeError BinaryProps
decodeBinary capabilityJson = do
  obj <- decodeJson capabilityJson
  valueOn <- obj .: "value_on"
  valueOff <- obj .: "value_off"
  valueToggle <- obj .:? "value_toggle"
  pure { valueOn, valueOff, valueToggle }

decodeEnum :: Json -> Either JsonDecodeError EnumProps
decodeEnum capabilityJson = do
  obj <- decodeJson capabilityJson
  values <- obj .: "values"
  pure { values }

decodeNumeric :: Json -> Either JsonDecodeError NumericProps
decodeNumeric capabilityJson = do
  obj <- decodeJson capabilityJson
  valueMax <- obj .:? "value_max"
  valueMin <- obj .:? "value_min"
  valueStep <- obj .:? "value_step"
  unit <- obj .:? "unit"
  pure { valueMax, valueMin, valueStep, unit }

decodeExposes :: Maybe String -> Array Json -> Either JsonDecodeError (Array Exposes)
decodeExposes featureType exposesJson = do
  fold <$>
    --
    -- for
    --   :: Array exposesJson
    --   -> (exposesJson -> Either JsonDecodeError (Array Exposes)
    --   -> Either JsonDecodeError (Array (Array Exposes))
    --
    (for exposesJson $ \e -> do
        obj <- decodeJson e
        features :: Maybe (Array Json) <- obj .:? "features"
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
      mFeatures :: Maybe (Array Json) <- obj .:? "features"
      mItemType :: Maybe Json <- obj .:? "item_type"

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
          composite <- for features $ decodeExposes' Nothing
          pure $ exposed { subProps = Composite $ composite }
        "list", _, Just itemType -> do
          traceM itemType
          listSubProps <- decodeExposes' Nothing itemType
          pure $ exposed { subProps = List { itemType: listSubProps } }
        _, _, _ -> pure $ exposed { subProps = Null {} }
