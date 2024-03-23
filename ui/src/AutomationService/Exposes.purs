module AutomationService.Exposes
  ( BinaryProps
  , Exposes
--  , CompositeExposes(..)
--  , CompositeProps
  , EnumProps
--  , ListExposes(..)
--  , ListProps
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

import Debug (trace)

import Prelude (class Eq, class Show, bind, const, pure, show, (<<<), ($), (<$>), (=<<), (>), flip, (<>))

import Control.Alternative ((<|>))
import Data.Argonaut (Json, JsonDecodeError, decodeJson, toArray)
import Data.Argonaut.Decode.Combinators ((.:), (.:?))
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Decoders (decodeBoolean, decodeString)
import Data.Int.Bits ((.&.))
import Data.Either (Either, fromRight, either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
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

data SubProps
  = Binary BinaryProps
  | Enum EnumProps
  | Numeric NumericProps

type Exposes =
  { capType     :: String
  , featureType :: Maybe String
  , name        :: String
  , description :: Maybe String
  , label       :: String
  -- Maybe only because it doesn't exist inside list type 🤮
  , property    :: Maybe String
  , access      :: Int
  , subProps    :: Maybe SubProps
  }


-- Composite

-- CompositeExposes is a subset of Exposes values that include
-- everything but composite or list types
--- data CompositeExposes
---   = CompGenericCap (ExposesBase ())
---   | CompBinaryCap  (ExposesBase BinaryProps)
---   | CompEnumCap    (ExposesBase EnumProps)
---   | CompNumericCap (ExposesBase NumericProps)
--- 
--- derive instance Generic CompositeExposes _
--- 
--- instance Show CompositeExposes where
---   show = genericShow
--- 
--- toCompositeFromExposes :: Exposes -> CompositeExposes
--- toCompositeFromExposes = case _ of
---   GenericCap cap -> CompGenericCap cap
---   BinaryCap cap -> CompBinaryCap cap
---   EnumCap cap -> CompEnumCap cap
---   NumericCap cap -> CompNumericCap cap
---   CompositeCap cap -> CompGenericCap $ delete (Proxy :: Proxy "features") cap
---   ListCap cap -> CompGenericCap $ delete (Proxy :: Proxy "itemType") cap
--- 
--- type CompositeProps = (features :: Array CompositeExposes)
--- 
--- 
--- -- List
--- 
--- -- ListExposes is a subset of Exposes values that don't
--- -- include list types
--- data ListExposes
---   = ListGenericCap   (ExposesBase ())
---   | ListBinaryCap    (ExposesBase BinaryProps)
---   | ListEnumCap      (ExposesBase EnumProps)
---   | ListNumericCap   (ExposesBase NumericProps)
---   | ListCompositeCap (ExposesBase CompositeProps)
--- 
--- derive instance Generic ListExposes _
--- 
--- instance Show ListExposes where
---   show = genericShow
--- 
--- toListFromExposes :: Exposes -> ListExposes
--- toListFromExposes = case _ of
---   GenericCap cap -> ListGenericCap $ cap
---   BinaryCap cap -> ListBinaryCap $ cap
---   EnumCap cap -> ListEnumCap $ cap
---   NumericCap cap -> ListNumericCap $ cap
---   CompositeCap cap -> ListCompositeCap cap
---   ListCap cap ->
---     ListGenericCap $ delete (Proxy :: Proxy "itemType") cap
--- 
--- type ListProps = (itemType :: ListExposes)


-- Exposes

-- data Exposes
--   = GenericCap   (ExposesBase ())
--   | BinaryCap    (ExposesBase BinaryProps)
--   | EnumCap      (ExposesBase EnumProps)
--   | NumericCap   (ExposesBase NumericProps)
--   -- not sure there is any value in explicitly creating a 'text'
--   -- type, since it has the same properties as ExposesBase
--   | CompositeCap (ExposesBase CompositeProps)
--   | ListCap      (ExposesBase ListProps)
-- 
-- derive instance Generic Exposes _
-- 
-- instance Show Exposes where
--   show = genericShow
-- 
-- getBaseExposes :: Exposes -> ExposesBase ()
-- getBaseExposes = case _ of
--   GenericCap r -> r
--   BinaryCap r -> getBaseExposes r
--   EnumCap  r -> getBaseExposes r
--   NumericCap r -> getBaseExposes r
--   CompositeCap r -> getBaseExposes r
--   ListCap r -> getBaseExposes r
--   where
--     getBaseExposes :: forall r. ExposesBase r -> ExposesBase ()
--     getBaseExposes { capType, featureType, name, description, label, property, access } =
--       { capType, featureType, name, description, label, property, access }

decodeBaseExposes
  :: Maybe String -> Json -> Either JsonDecodeError Exposes
decodeBaseExposes featureType capabilityJson = do
  obj <- decodeJson capabilityJson
  capType <- obj .: "type"
  name <- obj .: "name"
  description <- obj .:? "description"
  label <- obj .: "label"
  property <- obj .:? "property"
  access <- obj .: "access"
  pure { capType, featureType, name, description, label, property, access, subProps: Nothing }


-- decodeBaseExposes
--   :: Maybe String -> Json -> Either JsonDecodeError (ExposesBase ())
-- decodeBaseExposes featureType capabilityJson = do
--   obj <- decodeJson capabilityJson
--   capType <- obj .: "type"
--   name <- obj .: "name"
--   description <- obj .:? "description"
--   label <- obj .: "label"
--   property <- obj .:? "property"
--   access <- obj .: "access"
--   pure { capType, featureType, name, description, label, property, access }

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

-- decodeExposes :: Maybe String -> Json -> Either JsonDecodeError Exposes
-- decodeExposes featureType capabilityJson = do
--   obj <- decodeJson capabilityJson
--   baseCap <- decodeBaseExposes featureType capabilityJson
--   mFeatures <- obj .:? "features"
--   mItemType <- obj .:? "item_type"
-- 
--   let
--     fromRightCap
--       :: forall r. (r -> Exposes) -> Either JsonDecodeError r -> Exposes
--     fromRightCap = either (const $ GenericCap baseCap)
--     _ = flip (maybe "") baseCap.featureType $ \ft ->
--          let _ = trace ("baseCap.featureType: " <> ft <> ", baseCap.capType: " <> baseCap.capType <> ", baseCap.name: " <> baseCap.name) $ \_ -> ""
--          in ft
-- 
--   case baseCap.capType, mItemType, toArray =<< mFeatures of
--     "binary", _, _ -> pure $
--       fromRightCap (BinaryCap <<< merge baseCap) (decodeBinary capabilityJson)
--     "enum", _, _ -> pure $
--       fromRightCap (EnumCap <<< merge baseCap) (decodeEnum capabilityJson)
--     "numeric", _ , _ -> pure $
--       fromRightCap (NumericCap <<< merge baseCap) (decodeNumeric capabilityJson)
--     "composite", _ , Just features' ->
--       let
--         caps = fromRight [] $ sequence $ decodeExposes Nothing <$> features'
--         features = toCompositeFromExposes <$> caps
--       in
--         pure $ CompositeCap $ merge baseCap { features }
--     "list", Just itemType', _ ->
--       let
--         cap = toListFromExposes <$> decodeExposes Nothing itemType'
--       in
--         pure $ either
--          (const $ GenericCap baseCap)
--          (\itemType -> ListCap $ merge baseCap { itemType })
--          cap
--     _, _, _ -> pure $ GenericCap baseCap

decodeExposes :: Maybe String -> Json -> Either JsonDecodeError Exposes
decodeExposes featureType capabilityJson = do
  obj <- decodeJson capabilityJson
  exposed <- decodeBaseExposes featureType capabilityJson

  mFeatures :: Maybe (Array Json) <- obj .:? "features"

  -- mItemType <- obj .:? "item_type"

  case exposed.capType, mFeatures of 
    "binary", _ -> pure exposed
    "enum", _ -> pure exposed
    _, _ -> pure exposed
