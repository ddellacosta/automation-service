{-# LANGUAGE TemplateHaskell #-}

module Service.Automation
  ( _ValueMsg
  , Automation(..)
  , ClientMsg(..)
  , Message(..)
  , ServerMsg(..)
  , name
  , nullAutomation
  , startTime
  )
  where

import Control.Lens (Lens', lens, makePrisms)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (Object), toJSONList, withObject)
import Data.Aeson.Types (Parser)
import Data.ByteString.Char8 (ByteString)
import Data.Either (rights)
import qualified Data.Text.Encoding as T
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Service.AutomationName (AutomationName (..))
import UnliftIO.STM (TChan)

data ClientMsg
  = ByteStringMsg [ByteString]
  | ValueMsg Value
  | Shutdown
  deriving (Eq, Generic, Show)

makePrisms ''ClientMsg

instance ToJSON ClientMsg where
  toJSON (ByteStringMsg msgBSs) = toJSONList . rights $ T.decodeUtf8' <$> msgBSs
  toJSON (ValueMsg v)           = v

instance FromJSON ClientMsg where
  parseJSON :: Value -> Parser ClientMsg
  parseJSON = withObject "ClientMsg" (pure . ValueMsg . Object)

data ServerMsg
  = ServerMsg Value
  deriving (Eq, Generic, Show)

data Message
  = Client AutomationName ClientMsg
  | Server AutomationName ServerMsg
  deriving (Show)

data Automation monad = Automation
  { _name      :: AutomationName
  , _cleanup   :: TChan Message -> monad ()
  , _run       :: TChan Message -> monad ()
  , _startTime :: UTCTime
  }

name :: Lens' (Automation m) AutomationName
name = lens _name (\am newName -> am { _name = newName })

startTime :: Lens' (Automation m) UTCTime
startTime = lens _startTime (\am newStartTime -> am { _startTime = newStartTime })

nullAutomation :: (Applicative m) => UTCTime -> Automation m
nullAutomation ts = Automation Null noop noop ts
  where
    noop = const $ pure ()
