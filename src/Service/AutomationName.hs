module Service.AutomationName
  ( AutomationName(..)
  , Port(..)
  , parseAutomationName
  , parseAutomationNameText
  , serializeAutomationName
  )
where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Char (isDigit, isLower, isSpace)
import Data.Hashable (Hashable (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import qualified Text.ParserCombinators.ReadP as RP
import Text.Read (readMaybe)

newtype Port = Port Natural
  deriving (Generic, Eq, Ord, Show, Enum, Integral, Num, Real, Hashable, FromJSON)

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
  httpOrLuaScript ->
    let
      parsed = RP.readP_to_S (parseHTTP <|> parseLuaScript) httpOrLuaScript
    in
      if null parsed then
        Nothing
      else
        fst . head $ parsed

parseAutomationNameText :: Text -> Maybe AutomationName
parseAutomationNameText = parseAutomationName . T.unpack

parseHTTP :: RP.ReadP (Maybe AutomationName)
parseHTTP = do
  _http <- RP.string "HTTP"
  _space <- RP.string " "
  port <- RP.munch1 isDigit
  pure $ HTTP . Port <$> (readMaybe port :: Maybe Natural)

parseLuaScript :: RP.ReadP (Maybe AutomationName)
parseLuaScript = do
  firstIsLower <- RP.satisfy isLower
  scriptName <-
    (firstIsLower:) <$> RP.manyTill RP.get (RP.satisfy isSpace *> pure () <|> RP.eof)
  pure $ Just (LuaScript scriptName)
