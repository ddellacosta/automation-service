{-# LANGUAGE DerivingStrategies #-}

module Service.AutomationName
  ( AutomationName(..)
  , Port(..)
  , parseAutomationName
  , parseAutomationNameText
  , serializeAutomationName
  )
where

import Control.Monad (guard)
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Char as Char
import Data.Hashable (Hashable (..))
import Data.List (uncons)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

newtype Port = Port Natural
  deriving (Generic, Eq, Ord, Show)
  deriving newtype (Enum, Integral, Num, Real)

instance Hashable Port
instance FromJSON Port

data AutomationName
  = Gold
  | HTTP Port
  | HTTPDefault
  | LuaScript FilePath
  | Null
  | StateManager
  deriving (Generic, Show, Eq, Ord)

instance Hashable AutomationName

instance ToJSON AutomationName where
  toJSON (LuaScript fp) = toJSON $ T.pack fp
  toJSON autoName       = toJSON . T.pack . show $ autoName

instance FromJSON AutomationName

serializeAutomationName :: AutomationName -> Text
serializeAutomationName = \case
  (LuaScript filepath) -> T.pack filepath
  autoName             -> T.pack . show $ autoName

parseAutomationName :: String -> Maybe AutomationName
parseAutomationName = \case
  "Gold" -> Just Gold
  "Null" -> Just Null
  maybeLuaScript -> do
    let filepath = filter (/= '"') maybeLuaScript
    (firstChar, _remainder) <- uncons filepath
    guard (Char.isLower firstChar) *>
      (pure . LuaScript $ filepath)

parseAutomationNameText :: Text -> Maybe AutomationName
parseAutomationNameText = parseAutomationName . T.unpack
