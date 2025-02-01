module AutomationService.Exposes
  ( BinaryProps
  , Capability
  , CapType(..)
  , CompositeProps
  , EnumProps
  , Exposes
  , FeatureType(..)
  , ListProps
  , NumericProps
  , SubProps(..)
  , ValueOnOff(..)
  , _access
  , _description
  , _featureType
  , _label
  , _name
  , _property
  , _subProps
  , _type
  , canGet
  , canSet
  , decodeCapability
  , decodeExposes
  , enumValues
  , isOn
  , isPublished
  , serializeValueOnOff
  )
where

import Control.Alternative ((<|>))
import Data.Argonaut (Json, JsonDecodeError, decodeJson)
import Data.Argonaut.Decode.Combinators ((.:), (.:?))
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Decoders (decodeBoolean, decodeString)
import Data.Array.NonEmpty (NonEmptyArray, cons', fromArray, head, tail)
import Data.Int.Bits ((.&.))
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (for)
import Prelude (class Eq, class Ord, class Show, ($), (<>), (<$>), (>>=), (=<<), (>), (==), bind, const, pure, show)
import Type.Proxy (Proxy(..))


-- Capability.type

data CapType
  = Binary'
  | Composite'
  | Enum'
  | List'
  | Numeric'
  | UnknownCT String

derive instance Eq CapType
derive instance Generic CapType _
derive instance Ord CapType -- for Device's Ord

instance Show CapType where
  show = genericShow

instance DecodeJson CapType where
  decodeJson :: Json -> Either JsonDecodeError CapType
  decodeJson json = decodeString json >>= \ct ->
    pure $ case ct of
      "binary" -> Binary'
      "composite" -> Composite'
      "enum" -> Enum'
      "list" -> List'
      "numeric" -> Numeric'
      unknown -> UnknownCT unknown


-- Capability.featureType

data FeatureType
  = Cover
  | Light
  | UnknownFT String

derive instance Eq FeatureType
derive instance Generic FeatureType _
derive instance Ord FeatureType -- for Device's Ord

instance Show FeatureType where
  show = genericShow

instance DecodeJson FeatureType where
  decodeJson :: Json -> Either JsonDecodeError FeatureType
  decodeJson json = decodeString json >>= \ft ->
    pure $ case ft of
      "cover" -> Cover
      "light" -> Light
      unknown -> UnknownFT unknown


-- Capability.access

-- |
-- | https://www.zigbee2mqtt.io/guide/usage/exposes.html#access
-- |
isPublished :: Int -> Boolean
isPublished a = 1 .&. a > 0

-- |
-- | https://www.zigbee2mqtt.io/guide/usage/exposes.html#access
-- |
canSet :: Int -> Boolean
canSet a = 2 .&. a > 0

-- |
-- | canGet implies isPublished
-- |
-- | https://www.zigbee2mqtt.io/guide/usage/exposes.html#access
-- |
canGet :: Int -> Boolean
canGet a = 4 .&. a > 0


-- Capability.subProps

-- |
-- | "Why can't you just be normal?"
-- |
-- | https://www.zigbee2mqtt.io/guide/usage/exposes.html#binary
-- |
data ValueOnOff = ValueOnOffBool Boolean | ValueOnOffString String

derive instance Eq ValueOnOff
derive instance Generic ValueOnOff _
derive instance Ord ValueOnOff -- for Device's Ord

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

isOn :: Capability -> ValueOnOff -> Boolean
isOn { subProps } = case subProps of
  Binary { valueOn } -> (_ == valueOn)
  _ -> const false

type BinaryProps =
  { valueOn     :: ValueOnOff
  , valueOff    :: ValueOnOff
  , valueToggle :: Maybe String
  } 

type CompositeProps = Array Capability

type EnumProps = { values :: Array String }

enumValues :: Capability -> Array String
enumValues { subProps } = case subProps of
  Enum { values } -> values
  _ -> []

type ListProps = Capability

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

derive instance Eq SubProps
derive instance Generic SubProps _
derive instance Ord SubProps -- for Device's Ord

instance Show SubProps where
  -- apparently due to the fact that SubProps can be recursive
  -- via ListProps or CompositeProps, point-free here introduces a
  -- cycle that won't type-check, I don't really understand why yet
  -- though (TODO)
  show sp = genericShow sp


-- Capability

type Capability =
  { type        :: CapType
  , featureType :: Maybe FeatureType
  , name        :: String
  , description :: Maybe String
  , label       :: String
  -- Maybe only because it doesn't exist inside list type 🤮
  , property    :: Maybe String
  , access      :: Int
  , subProps    :: SubProps
  }

--
-- lens boilerplate
--
_type
  :: forall a b r. Lens { type :: a | r } { type :: b | r } a b
_type = prop (Proxy :: Proxy "type")

_featureType
  :: forall a b r. Lens { featureType :: a | r } { featureType :: b | r } a b
_featureType = prop (Proxy :: Proxy "featureType")

_name
  :: forall a b r. Lens { name :: a | r } { name :: b | r } a b
_name = prop (Proxy :: Proxy "name")

_description
  :: forall a b r. Lens { description :: a | r } { description :: b | r } a b
_description = prop (Proxy :: Proxy "description")

_label
  :: forall a b r. Lens { label :: a | r } { label :: b | r } a b
_label = prop (Proxy :: Proxy "label")

_property
  :: forall a b r. Lens { property :: a | r } { property :: b | r } a b
_property = prop (Proxy :: Proxy "property")

_access
  :: forall a b r. Lens { access :: a | r } { access :: b | r } a b
_access = prop (Proxy :: Proxy "access")

_subProps
  :: forall a b r. Lens { subProps :: a | r } { subProps :: b | r } a b
_subProps = prop (Proxy :: Proxy "subProps")

--

-- Exposes

type Exposes = NonEmptyArray Capability


--
-- Parsing
--

decodeBaseCapability
  :: Maybe FeatureType -> Json -> Either JsonDecodeError Capability
decodeBaseCapability featureType capabilityJson = do
  obj <- decodeJson capabilityJson
  type' <- obj .: "type"
  name <- obj .: "name"
  description <- obj .:? "description"
  label <- obj .: "label"
  property <- obj .:? "property"
  access <- obj .: "access"
  pure
    { type: type'
    , featureType
    , name
    , description
    , label
    , property
    , access
    , subProps: Null
    }

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

decodeCapability :: Maybe FeatureType -> Json -> Either JsonDecodeError Capability
decodeCapability featureType capabilityJson = do
  obj <- decodeJson capabilityJson
  exposed <- decodeBaseCapability featureType capabilityJson
  mFeatures <- obj .:? "features"
  mItemType <- obj .:? "item_type"

  case exposed.type, mFeatures, mItemType of
    Binary', _, _ -> do
      binary <- Binary <$> decodeBinary capabilityJson
      pure $ exposed { subProps = binary }
    Enum', _, _ -> do
      enum <- Enum <$> decodeEnum capabilityJson
      pure $ exposed { subProps = enum }
    Numeric', _, _ -> do
      numeric <- Numeric <$> decodeNumeric capabilityJson
      pure $ exposed { subProps = numeric }
    Composite', Just features, _ -> do
      composite <- Composite <$> (for features $ decodeCapability Nothing)
      pure $ exposed { subProps = composite }
    List', _, Just itemType -> do
      list <- List <$> decodeCapability Nothing itemType
      pure $ exposed { subProps = list }
    _, _, _ -> pure exposed -- subProps = Null per decodeBaseCapability


--
-- WHY DOES THIS TAKE featureType !?!?!
--

-- |
-- | This flattens out the features array into the same array that
-- | individual 'exposes' entries are returned in. Zigbee2MQTT
-- | distinguishes these as 'specific' (the former) vs. 'generic' (the
-- | latter). However, I don't see much value in separating them like
-- | this given how we'll be looking this data up.
-- |
-- | https://www.zigbee2mqtt.io/guide/usage/exposes.html
-- |
decodeExposes
  :: Maybe FeatureType -> NonEmptyArray Json -> Either JsonDecodeError Exposes
decodeExposes _featureType' exposesJson = do
  --
  -- for
  --   :: Array capabilityJson
  --   -> (capabilityJson -> Either JsonDecodeError Exposes)
  --   -> Either JsonDecodeError (NonEmptyArray Exposes))
  --
  exposesArrays <- for exposesJson $ \e -> do
    obj <- decodeJson e
    features <- obj .:? "features"
    featureType' <- obj .:? "type"
    case fromArray =<< features of
      Just features' -> for features' $ decodeCapability featureType'
      _ -> for (e `cons'` []) $ decodeCapability featureType'

  foldM (\exposes nextExposes -> Right $ exposes <> nextExposes)
    (head exposesArrays)
    (tail exposesArrays)
