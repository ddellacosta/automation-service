module Service.ActionName
  ( ActionName(..)
  , parseActionName
  , serializeActionName
  )
where

import Data.Aeson (FromJSON(..), ToJSON(..), defaultOptions, genericToEncoding)
import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics (Generic)

data ActionName
  = Null
  | Gold
  | Chrizmaz
  | Trinity
  deriving (Generic, Show, Eq, Ord)

instance ToJSON ActionName where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ActionName

serializeActionName :: ActionName -> Text
serializeActionName = T.pack . show 

parseActionName :: Text -> Maybe ActionName
parseActionName = \case
  "Gold" -> Just Gold
  "Chrizmaz" -> Just Chrizmaz
  "Trinity" -> Just Trinity
  _ -> Nothing
