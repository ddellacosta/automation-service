module AutomationService.Capability
  ( BinaryProps
  , Capability(..)
  , CapabilityBase
  , CompositeCapability(..)
  , CompositeProps
  , EnumProps
  , ListCapability(..)
  , ListProps
  , NumericProps
  , ValueOnOff(..)
  , canGet
  , canSet
  , decodeCapability
  , isPublished
  )
where

import Prelude (class Show, bind, const, pure, ($), (<$>), (=<<), (>))

import Control.Alternative ((<|>))
import Data.Argonaut (Json, JsonDecodeError, decodeJson, toArray)
import Data.Argonaut.Decode.Combinators ((.:), (.:?))
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Decoders (decodeBoolean, decodeString)
import Data.Int.Bits ((.&.))
import Data.Either (Either, fromRight, either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
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

data ValueOnOff = ValueOnOffBool Boolean | ValueOnOffString String

derive instance Generic ValueOnOff _

instance DecodeJson ValueOnOff where
  decodeJson :: Json -> Either JsonDecodeError ValueOnOff
  decodeJson json =
    ValueOnOffBool <$> decodeBoolean json <|> ValueOnOffString <$> decodeString json

instance Show ValueOnOff where
  show = genericShow

-- Base type properties

type CapabilityBase r =
  { capType     :: String
  , featureType :: Maybe String
  , name        :: String
  , description :: Maybe String
  , label       :: String
  -- Maybe only because it doesn't exist inside list type 🤮
  , property    :: Maybe String
  , access      :: Int
  -- todo presets
  | r
  }

type BinaryProps =
  ( valueOn     :: ValueOnOff
  , valueOff    :: ValueOnOff
  , valueToggle :: Maybe String
  )

type EnumProps = (values :: Array String)

type NumericProps =
  ( valueMax  :: Maybe Int
  , valueMin  :: Maybe Int
  , valueStep :: Maybe Int
  , unit      :: Maybe String
  )


-- Composite

-- CompositeCapability is a subset of Capability values that include
-- everything but composite or list types
data CompositeCapability
  = CompGenericCap (CapabilityBase ())
  | CompBinaryCap  (CapabilityBase BinaryProps)
  | CompEnumCap    (CapabilityBase EnumProps)
  | CompNumericCap (CapabilityBase NumericProps)

derive instance Generic CompositeCapability _

instance Show CompositeCapability where
  show = genericShow

toCompositeFromCapability :: Capability -> CompositeCapability
toCompositeFromCapability = case _ of
  GenericCap cap -> CompGenericCap cap
  BinaryCap cap -> CompBinaryCap cap
  EnumCap cap -> CompEnumCap cap
  NumericCap cap -> CompNumericCap cap
  CompositeCap cap -> CompGenericCap $ delete (Proxy :: Proxy "features") cap
  ListCap cap -> CompGenericCap $ delete (Proxy :: Proxy "itemType") cap

type CompositeProps = (features :: Array CompositeCapability)


-- List

-- ListCapability is a subset of Capability values that don't
-- include list types
data ListCapability
  = ListGenericCap   (CapabilityBase ())
  | ListBinaryCap    (CapabilityBase BinaryProps)
  | ListEnumCap      (CapabilityBase EnumProps)
  | ListNumericCap   (CapabilityBase NumericProps)
  | ListCompositeCap (CapabilityBase CompositeProps)

derive instance Generic ListCapability _

instance Show ListCapability where
  show = genericShow

toListFromCapability :: Capability -> ListCapability
toListFromCapability = case _ of
  GenericCap cap -> ListGenericCap $ cap
  BinaryCap cap -> ListBinaryCap $ cap
  EnumCap cap -> ListEnumCap $ cap
  NumericCap cap -> ListNumericCap $ cap
  CompositeCap cap -> ListCompositeCap cap
  ListCap cap ->
    ListGenericCap $ delete (Proxy :: Proxy "itemType") cap

-- I need to be able to generate everything else before I try this
type ListProps = (itemType :: ListCapability)


-- Capability

data Capability
  = GenericCap   (CapabilityBase ())
  | BinaryCap    (CapabilityBase BinaryProps)
  | EnumCap      (CapabilityBase EnumProps)
  | NumericCap   (CapabilityBase NumericProps)
  -- not sure there is any value in explicitly creating a 'text'
  -- type, since it has the same properties as CapabilityBase
  -- todo composite
  | CompositeCap (CapabilityBase CompositeProps)
  | ListCap      (CapabilityBase ListProps)

derive instance Generic Capability _

instance Show Capability where
  show = genericShow


decodeBaseCapability
  :: Maybe String -> Json -> Either JsonDecodeError (CapabilityBase ())
decodeBaseCapability featureType capabilityJson = do
  obj <- decodeJson capabilityJson
  capType <- obj .: "type"
  name <- obj .: "name"
  description <- obj .:? "description"
  label <- obj .: "label"
  property <- obj .:? "property"
  access <- obj .: "access"
  pure { capType, featureType, name, description, label, property, access }

decodeBinary :: Json -> Either JsonDecodeError (Record BinaryProps)
decodeBinary capabilityJson = do
  obj <- decodeJson capabilityJson
  valueOn <- obj .: "value_on"
  valueOff <- obj .: "value_off"
  valueToggle <- obj .:? "value_toggle"
  pure { valueOn, valueOff, valueToggle }

decodeEnum :: Json -> Either JsonDecodeError (Record EnumProps)
decodeEnum capabilityJson = do
  obj <- decodeJson capabilityJson
  values <- obj .: "values"
  pure { values }

decodeNumeric :: Json -> Either JsonDecodeError (Record NumericProps)
decodeNumeric capabilityJson = do
  obj <- decodeJson capabilityJson
  valueMax <- obj .:? "value_max"
  valueMin <- obj .:? "value_min"
  valueStep <- obj .:? "value_step"
  unit <- obj .:? "unit"
  pure { valueMax, valueMin, valueStep, unit }

decodeCapability :: Maybe String -> Json -> Either JsonDecodeError Capability
decodeCapability featureType capabilityJson = do
  obj <- decodeJson capabilityJson
  baseCap <- decodeBaseCapability featureType capabilityJson
  mFeatures <- obj .:? "features"
  mItemType <- obj .:? "item_type"

  case baseCap.capType, mItemType, toArray =<< mFeatures of
    "binary", _, _ -> pure $ either
      (const $ GenericCap baseCap)
      (\binaryFields -> BinaryCap $ merge baseCap binaryFields)
      (decodeBinary capabilityJson)
    "enum", _, _ -> pure $ either
      (const $ GenericCap baseCap)
      (\enumFields -> EnumCap $ merge baseCap enumFields)
      (decodeEnum capabilityJson)
    "numeric", _ , _ -> pure $ either
      (const $ GenericCap baseCap)
      (\numericFields -> NumericCap $ merge baseCap numericFields)
      (decodeNumeric capabilityJson)
    "composite", _ , Just features' ->
      let
        caps = fromRight [] $ sequence $ decodeCapability Nothing <$> features'
        features = toCompositeFromCapability <$> caps
      in
        pure $ CompositeCap $ merge baseCap { features }
    "list", Just itemType', _ ->
      let
        cap = toListFromCapability <$> decodeCapability Nothing itemType'
      in
        pure $ either
         (const $ GenericCap baseCap)
         (\itemType -> ListCap $ merge baseCap { itemType })
         cap
    _, _, _ -> pure $ GenericCap baseCap