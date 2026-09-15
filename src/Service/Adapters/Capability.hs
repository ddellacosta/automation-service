{-# LANGUAGE TemplateHaskell #-}

module Service.Adapters.Capability
  ( Access(..)
  , Capability(..)
  , Capabilities
  , ItemType(..)
  , Kind(..)
  , NumericPreset(..)
  , Property
  , _Binary
  , _Composite
  , _Enum
  , _List
  , _Numeric
  , _Text
  , access
  , attachCapabilityKind
  , description
  , itemType
  , kind
  , label
  , lengthMax
  , lengthMin
  , name
  , pDescription
  , pName
  , pValue
  , parseCapabilities
  , parseKind
  , presets
  , property
  , readable
  , reports
  , toAccess
  , unit
  , valueMax
  , valueMin
  , valueOff
  , valueOn
  , valueStep
  , valueToggle
  , values
  , writeable
  )
where

import Control.Lens (makeClassyPrisms, makeFieldsNoPrefix)
import Data.Aeson (Array, FromJSON(..), Object, (.:), (.:?), withObject)
import Data.Aeson.Types (Parser, Value(..), parseEither)
import Data.Bits ((.&.))
import Data.Either (Either(..))
import Data.Foldable (foldl')
import qualified Data.HashMap.Strict as M
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable (..))
import Data.Maybe (Maybe(..))
import qualified Data.Text as T
import Data.Traversable (for, forAccumM)
import Data.Vector (toList)
import GHC.Generics (Generic)
import Prelude (Bool, Double, Eq, Int, Show, String, ($), (<$>), (<*>), (=<<), (>), (<>), flip, pure)

data NumericPreset = NumericPreset
  { _pName :: T.Text
  , _pValue :: Int
  , _pDescription :: T.Text
  } deriving (Eq, Show)

makeFieldsNoPrefix ''NumericPreset

data ItemType = ItemType
  { _itName :: T.Text
  , _itLabel :: T.Text
  , _itDescription :: Maybe T.Text
  , _itKind :: Kind
  } deriving (Eq, Show)

-- makeFieldsNoPrefix ''ItemType

data Kind
  = Binary
    { _valueOn :: T.Text
    , _valueOff :: T.Text
    , _valueToggle :: Maybe T.Text
    }
  | Composite -- will these show up at all?
  | Enum
    { _values :: [ T.Text ] }
  | List
    { _itemType :: ItemType
    , _lengthMin :: Maybe Int
    , _lengthMax :: Maybe Int
    } 
  | Numeric
    { _unit :: Maybe T.Text
    , _valueMax :: Maybe Int
    , _valueMin :: Maybe Int
    , _valueStep :: Maybe Double
    , _presets :: [NumericPreset]
    }
  | Text
  deriving (Eq, Show)

makeFieldsNoPrefix ''Kind
makeClassyPrisms ''Kind

-- |
-- | https://www.zigbee2mqtt.io/guide/usage/exposes.html#access
-- |
data Access
  = Reports
  | Writeable
  | Readable -- implies Reports as well for Zigbee2MQTT, but may not for Matter
  deriving (Eq, Generic, Show)

instance Hashable Access

reports, writeable, readable :: Int -> Bool
reports a = 1 .&. a > 0
writeable a = 2 .&. a > 0
readable a = 4 .&. a > 0

toAccess :: Int -> HashSet Access
toAccess access =
  foldl'
  (\accessSet (accessPred, accessVal) ->
     if accessPred access
     then
       HS.insert accessVal accessSet
     else
       accessSet
  )
  HS.empty
  [(reports, Reports), (writeable, Writeable), (readable, Readable)]

data Capability = Capability
  { _name :: T.Text
  , _label :: T.Text
  , _property :: T.Text
  , _description :: Maybe T.Text
  , _kind :: Kind
  , _access :: HashSet Access
  } deriving (Eq, Show)

makeFieldsNoPrefix ''Capability

type Capabilities = M.HashMap Property Capability

--
-- Capability.property
--
type Property = T.Text

instance FromJSON NumericPreset where
  parseJSON = withObject "NumericPreset" $ \np ->
    NumericPreset
    <$> np .: "name"
    <*> np .: "value"
    <*> np .: "description"

-- probably need to handle an exception here as this isn't exhaustive,
-- or make a null type or something or pick a default, but don't like
-- any of those options really
parseKind :: Object -> T.Text -> Parser Kind
parseKind capObj = \case
 "binary" -> do
   valueOn' <- capObj .: "value_on"
   valueOff' <- capObj .: "value_off"
   valueToggle' <- capObj .:? "value_toggle"
   pure $ Binary valueOn' valueOff' valueToggle'
 "composite" -> pure Composite
 "enum" -> Enum <$> capObj .: "values"
 "list" -> do
   itemType' <- capObj .: "item_type"
   lengthMin' <- capObj .:? "length_min"
   lengthMax' <- capObj .:? "length_max"
   parsedItemType <- parseJSON itemType'
   pure $ List parsedItemType lengthMin' lengthMax'
 "numeric" -> do
   unit' <- capObj .:? "unit"
   valueMax' <- capObj .:? "value_max"
   valueMin' <- capObj .:? "value_min"
   valueStep' <- capObj .:? "value_step"
   mPresets <- capObj .:? "presets"
   parsedPresets <- case mPresets of
     Just presets' -> for (toList presets') parseJSON
     Nothing -> pure []
   pure $ Numeric unit' valueMax' valueMin' valueStep' parsedPresets
 "text" -> pure Text

instance FromJSON ItemType where
  parseJSON = withObject "Capability" $ \c -> do
    name' <- c .: "name"
    label' <- c .: "label"
    description' <- c .:? "description"
    kind' <- parseKind c =<< c .: "type"
    pure $
      ItemType
        name'
        label'
        description'
        kind'

instance FromJSON Capability where
  parseJSON = withObject "Capability" $ \c -> do
    name' <- c .: "name"
    label' <- c .: "label"
    property' <- c .: "property"
    description' <- c .:? "description"
    kind' <- c .: "type"
    kindParsed <- parseKind c kind'
    access' <- c .: "access"
    pure $
      Capability
        name'
        label'
        property'
        description'
        kindParsed
        (toAccess access')

attachCapabilityKind :: Value -> Capability -> Parser [Capability]
attachCapabilityKind capObj capability =
  case capObj of 
    (Object c) -> do
      mFeatures :: Maybe Array <- c .:? "features"

      case mFeatures of
        Just features -> for (toList features) $ \c' -> do
          childCap <- parseJSON c'
          pure $ Capability
            ((_name capability) <> "-" <> (_name childCap))
            ((_label capability) <> "-" <> (_label childCap))
            ((_property capability) <> "." <> (_property childCap))
            (_description capability) 
            (_kind childCap)
            (_access childCap)

        Nothing -> pure $ [ capability ]

    _ -> pure [capability]

parseCapabilities :: Maybe Array -> Parser Capabilities
parseCapabilities mExposes = do
  let
    capabilities =
      case mExposes of
        Just exposes -> flip parseEither exposes $ \cs ->
          forAccumM M.empty (toList cs) $ \acc c' -> do
            let
              (cap :: Either String Capability) = parseEither parseJSON c'
            case cap of
              Right cap' -> do
                caps <- attachCapabilityKind c' cap'
                pure (foldl' (\acc' c -> M.insert (_property c) c acc') acc caps, ())
              Left _errorMsg -> do
                mFeatures <- withObject "features" (\features -> features .:? "features") c'
                caps <- parseCapabilities mFeatures
                pure (caps, ())
        Nothing -> Right (M.empty, [()])
  pure $ case capabilities of
    Right (capabilities', _unit) -> capabilities'
    Left _errorMsg -> M.empty
