module Service.AutomationName
  ( AutomationName(..)
  , parseAutomationName
  , serializeAutomationName
  )
where

import Data.Aeson (FromJSON(..), ToJSON(..), defaultOptions, genericToEncoding)
import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics (Generic)

data AutomationName
  = Null
  | LuaScript FilePath
  | Gold
  | Chrizmaz
  | Trinity
  | OnLow
  deriving (Generic, Show, Eq, Ord)

instance ToJSON AutomationName where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON AutomationName

serializeAutomationName :: AutomationName -> Text
serializeAutomationName = T.pack . show

parseAutomationName :: String -> Maybe AutomationName
parseAutomationName = \case
  "Gold" -> Just Gold
  "Chrizmaz" -> Just Chrizmaz
  "Trinity" -> Just Trinity
  "OnLow" -> Just OnLow
  maybeLuaScript -> case words maybeLuaScript of
    ["LuaScript", filePath] -> Just . LuaScript $ filePath
    _ -> Nothing
