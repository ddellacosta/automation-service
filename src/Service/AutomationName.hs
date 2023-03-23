module Service.AutomationName
  ( AutomationName(..)
  , parseAutomationName
  , parseAutomationNameText
  , serializeAutomationName
  )
where

import Data.Aeson (FromJSON(..), ToJSON(..), defaultOptions, genericToEncoding)
import Data.Hashable (Hashable(..))
import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics (Generic)

data AutomationName
  = Null
  | LuaScript FilePath
  | Gold
  | StateManager
  deriving (Generic, Show, Eq, Ord)

instance Hashable AutomationName

instance ToJSON AutomationName where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON AutomationName

serializeAutomationName :: AutomationName -> Text
serializeAutomationName = T.pack . show

parseAutomationName :: String -> Maybe AutomationName
parseAutomationName = \case
  "Null" -> Just Null
  "Gold" -> Just Gold
  "StateManager" -> Just StateManager
  maybeLuaScript -> case words maybeLuaScript of
    ["LuaScript", filePath] -> Just . LuaScript $ filter (/= '"') filePath
    _ -> Nothing

parseAutomationNameText :: Text -> Maybe AutomationName
parseAutomationNameText = parseAutomationName . T.unpack
