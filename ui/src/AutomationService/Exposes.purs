module AutomationService.Exposes
  ( BinaryProps
  , Capability(..)
  , CapabilityDetails
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
  , canGet
  , canSet
  , capabilities
  , capabilityDetails
  , decodeCapability
  , decodeExposes
  , enumValues
  , featureType
  , isOn
  , isPublished
  , matchingCapabilities
  , serializeValueOnOff
  )
where

import Control.Alt ((<|>))
import Data.Argonaut (Json, JsonDecodeError, decodeJson)
import Data.Argonaut.Decode.Combinators ((.:), (.:?))
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Decoders (decodeBoolean, decodeString)
import Data.Array (filter, length)
import Data.Array.NonEmpty (NonEmptyArray, cons', fromArray, head, tail)
import Data.Array.NonEmpty as NonEmpty
import Data.Int.Bits ((.&.))
import Data.Either (Either(..))
import Data.Foldable (foldM, null)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (for)
import Prelude (class Eq, class Ord, class Show, (<<<), (>>>), ($), (#), (<>), (<$>), (>>=), (=<<), (>), (==), bind, const, not, pure, show)
import Type.Proxy (Proxy(..))


-- CapabilityDetails.featureType

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


-- CapabilityDetails.access

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


-- CapabilityDetails.subProps

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

isOn :: CapabilityDetails -> ValueOnOff -> Boolean
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

enumValues :: CapabilityDetails -> Array String
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
  | Text NumericProps

derive instance Eq SubProps
derive instance Generic SubProps _
derive instance Ord SubProps -- for Device's Ord

instance Show SubProps where
  show sp = genericShow sp


-- CapabilityDetails

type CapabilityDetails =
  { featureType :: Maybe FeatureType
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

data Capability
  = Unknown CapabilityDetails
  | OnOff CapabilityDetails
  | Covering CapabilityDetails
  | Brightness CapabilityDetails
  | ColorTemperature CapabilityDetails
  | ColorTempStartup CapabilityDetails
  | ColorXY CapabilityDetails
  | ColorHue CapabilityDetails
  | ColorHex CapabilityDetails
  | ColorGradient CapabilityDetails
  | GradientScene CapabilityDetails
  | Hue CapabilityDetails
  | Saturation CapabilityDetails
  | IlluminanceLux CapabilityDetails
  | X CapabilityDetails
  | Y CapabilityDetails
  | Switch CapabilityDetails
  | Contact CapabilityDetails
  | Occupancy CapabilityDetails
  | OccupancyTimeout CapabilityDetails
  | Temperature CapabilityDetails
  | Humidity CapabilityDetails
  | AirQuality CapabilityDetails
  | VOC CapabilityDetails
  | Position CapabilityDetails
  -- not specified (i.e. manufacturer can do whatever)
  -- as of Matter 1.4
  | PowerOnBehavior CapabilityDetails
  | Effect CapabilityDetails
  | LEDControl CapabilityDetails
  | LinkQuality CapabilityDetails
  | Battery CapabilityDetails
  | BatteryLow CapabilityDetails
  | Tamper CapabilityDetails

capabilityDetails :: Capability -> CapabilityDetails
capabilityDetails = case _ of
  Unknown cd -> cd
  OnOff cd -> cd
  Covering cd -> cd
  Brightness cd -> cd
  ColorTemperature cd -> cd
  ColorTempStartup cd -> cd
  ColorXY cd -> cd
  ColorHue cd -> cd
  ColorHex cd -> cd
  ColorGradient cd -> cd
  GradientScene cd -> cd
  Hue cd -> cd
  Saturation cd -> cd
  IlluminanceLux cd -> cd
  X cd -> cd
  Y cd -> cd
  Switch cd -> cd
  Contact cd -> cd
  Occupancy  cd -> cd
  OccupancyTimeout cd -> cd
  Temperature  cd -> cd
  Humidity cd -> cd
  AirQuality cd -> cd
  VOC cd -> cd
  Position cd -> cd
  PowerOnBehavior cd -> cd
  Effect cd -> cd
  LEDControl cd -> cd
  LinkQuality cd -> cd
  Battery cd -> cd
  BatteryLow cd -> cd
  Tamper cd -> cd

derive instance Eq Capability
derive instance Generic Capability _
derive instance Ord Capability -- for Device's Ord

instance Show Capability where
  show sp = genericShow sp


-- Exposes

type Exposes = NonEmptyArray Capability


--
-- Parsing
--

decodeBaseCapabilityDetails
  :: Maybe FeatureType -> Json -> Either JsonDecodeError CapabilityDetails
decodeBaseCapabilityDetails featureType' capabilityJson = do
  obj <- decodeJson capabilityJson
  name <- obj .: "name"
  description <- obj .:? "description"
  label <- obj .: "label"
  property <- obj .:? "property"
  access <- obj .: "access"
  pure
    { featureType: featureType'
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

decodeCapabilityDetails :: Maybe FeatureType -> Json -> Either JsonDecodeError CapabilityDetails
decodeCapabilityDetails featureType' capabilityJson = do
  obj <- decodeJson capabilityJson
  baseCapabilityDetails <- decodeBaseCapabilityDetails featureType' capabilityJson
  mFeatures <- obj .:? "features"
  mType <- obj .:? "type"
  mItemType <- obj .:? "item_type"

  case mType, mFeatures, mItemType of
    Just "binary", _, _ -> do
      binary <- Binary <$> decodeBinary capabilityJson
      pure $ baseCapabilityDetails { subProps = binary }
    Just "enum", _, _ -> do
      enum <- Enum <$> decodeEnum capabilityJson
      pure $ baseCapabilityDetails { subProps = enum }
    Just "numeric", _, _ -> do
      numeric <- Numeric <$> decodeNumeric capabilityJson
      pure $ baseCapabilityDetails { subProps = numeric }
    Just "composite", Just features, _ -> do
      composite <- Composite <$> (for features $ decodeCapability Nothing)
      pure $ baseCapabilityDetails { subProps = composite }
    Just "list", _, Just itemType -> do
      list <- List <$> decodeCapability Nothing itemType
      pure $ baseCapabilityDetails { subProps = list }
    Just "text", _, _ -> do
      pure baseCapabilityDetails
    _, _, _ -> pure baseCapabilityDetails -- subProps = Null per decodeBaseCapabilityDetails


decodeCapability :: Maybe FeatureType -> Json -> Either JsonDecodeError Capability
decodeCapability featureType' capabilityJson = do
  details <- decodeCapabilityDetails featureType' capabilityJson

  Right $ case details.name, details.subProps of
    "state", Binary _ ->
      OnOff details
    -- hmm
    "state", Enum _ ->
      Covering details
    "brightness", Numeric _ ->
      Brightness details
    "color_temp", Numeric _ ->
      ColorTemperature details
    "color_temp_startup", Numeric _ ->
      ColorTempStartup details
    "color_xy", Composite _ ->
      ColorXY details
    "color_hs", Composite _ ->
      ColorHue details
    "hex", Numeric _ ->
      ColorHex details
    "hex", Text _ ->
      ColorHex details
    "gradient", List _ ->
      ColorGradient details
    "gradient_scene", Enum _ ->
      GradientScene details
    "hue", Numeric _ ->
      Hue details
    "saturation", Numeric _ ->
      Saturation details
    "illuminance_lux", Numeric _ ->
      IlluminanceLux details
    "x", Numeric _ ->
      X details
    "y", Numeric _ ->
      Y details
    "action", Enum _ ->
      Switch details
    "contact", Binary _ ->
      Contact details
    "occupancy", Binary _ ->
      Occupancy details
    "occupancy_timeout", Numeric _ ->
      OccupancyTimeout details
    "temperature", Numeric _ ->
      Temperature details
    "humidity", Numeric _ ->
      Humidity details
    "air_quality", Enum _ ->
      AirQuality details
    "voc", Numeric _ ->
      VOC details
    "position", Numeric _ ->
      Position details
    "power_on_behavior", Enum _ ->
      PowerOnBehavior details
    "effect", Enum _ ->
      Effect details
    "led_control", Enum _ ->
      LEDControl details
    "linkquality", Numeric _ ->
      LinkQuality details
    "battery", Numeric _ ->
      Battery details
    "battery_low", Binary _ ->
      BatteryLow details
    "tamper", Binary _ ->
      Tamper details
    _, _ ->
      Unknown details


-- |
-- | This flattens out the features array into the same array that
-- | individual 'exposes' entries are returned in. Zigbee2MQTT
-- | distinguishes these as 'specific' (the former) vs. 'generic' (the
-- | latter). However, I don't see much value in separating them like
-- | this given how we want to structure devices vs. capabilities.
-- |
-- | https://www.zigbee2mqtt.io/guide/usage/exposes.html
-- |
decodeExposes
  :: NonEmptyArray Json -> Either JsonDecodeError Exposes
decodeExposes exposesJson = do
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


-- utilities for filtering a collection of Capabilities a.k.a. Exposes

featureType :: Exposes -> FeatureType -> Boolean
featureType exposes ft =
  exposes #
    NonEmpty.filter ((\ft' -> ft' == Just ft) <<< _.featureType <<< capabilityDetails)
    >>> null
    >>> not

capabilities :: Exposes -> Array (CapabilityDetails -> Capability) -> Boolean
capabilities exposes toMatchCapabilities =
  length(matchingCapabilities exposes toMatchCapabilities) == length(toMatchCapabilities)

matchingCapabilities
  :: Exposes
  -> Array (CapabilityDetails -> Capability)
  -> Array (CapabilityDetails -> Capability)
matchingCapabilities exposes' toMatchCapabilities =
  filter
    (\capCons -> not <<< null <<< NonEmpty.filter (is capCons) $ exposes')
    toMatchCapabilities

  where
    is :: (CapabilityDetails -> Capability) -> Capability -> Boolean
    is cons v = v == (cons <<< capabilityDetails $ v)
