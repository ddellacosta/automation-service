module Service.AutomationName
  ( AutomationName(..)
  , parseAutomationName
  , parseAutomationNameText
  , serializeAutomationName
  )
where

import Control.Monad (guard)
import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Char as Char
import Data.List (uncons)
import Data.Hashable (Hashable(..))
import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics (Generic)

data AutomationName
  = Gold
  | HTTP
  | LuaScript FilePath
  | Null
  | StateManager
  deriving (Generic, Show, Eq, Ord)

instance Hashable AutomationName

instance ToJSON AutomationName where
  toJSON (LuaScript fp) = toJSON $ T.pack fp
  toJSON autoName = toJSON . T.pack . show $ autoName

instance FromJSON AutomationName

serializeAutomationName :: AutomationName -> Text
serializeAutomationName = \case
  (LuaScript filepath) -> T.pack filepath
  autoName -> T.pack . show $ autoName

parseAutomationName :: String -> Maybe AutomationName
parseAutomationName = \case
  "Gold" -> Just Gold
  "HTTP" -> Just HTTP
  "Null" -> Just Null
  "StateManager" -> Just StateManager
  maybeLuaScript -> do
    let filepath = filter (/= '"') maybeLuaScript
    (firstChar, _remainder) <- uncons filepath
    guard (Char.isLower firstChar) *>
      (pure . LuaScript $ filepath)

parseAutomationNameText :: Text -> Maybe AutomationName
parseAutomationNameText = parseAutomationName . T.unpack
