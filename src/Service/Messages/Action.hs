{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Service.Messages.Action
  ( Action(..)
  , ActionSchedule
  , _Null
  , _Schedule
  , _SendTo
  , _Start
  , _Stop
  , _StopServer
  )
where

import Control.Lens (makePrisms)
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value
  , (.:?)
  , defaultOptions
  , genericToEncoding
  , withObject
  )
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Service.ActionName (ActionName, parseActionName)

type ActionSchedule = Text

data Action where
  StopServer :: Action
  Start :: ActionName -> Action
  StartLua :: FilePath -> Action
  Stop :: ActionName -> Action
  SendTo :: ActionName -> Value -> Action
  Schedule :: Action -> ActionSchedule -> Action
  Null :: Action
  deriving (Generic, Eq, Show)

makePrisms ''Action

instance ToJSON Action where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Action where
  --
  -- The semantics of this are pretty stupid. Basically anything other
  -- than a perfectly well-formed message with a single action should
  -- be considered to provoke undefined behavior.
  --
  -- In reality, this will favor properties that come earlier in the
  -- `case` statement below, e.g. if you pass in a JSON value of
  --
  --   `{"stop": "MyFirstAction", "start": "MySecondAction"}
  --
  -- it will consistently start `MySecondAction` and `MyFirstAction`
  -- will never be stopped.
  --
  -- This is largely because this was the easiest thing to
  -- implement. If it turns out we really want to be able to pass
  -- multiple actions in a single message, I'll revisit this.
  --
  parseJSON :: Value -> Parser Action
  parseJSON = withObject "Action" $ \o -> do
    startAction <- o .:? "start"
    stopAction <- o .:? "stop"
    sendTo <- o .:? "send"
    msg <- o .:? "msg"
    luaScriptFilePath <- o .:? "filepath"
    pure $
      fromMaybe Null $
        case (startAction, stopAction, sendTo, luaScriptFilePath) of
          (Just "LuaScript", _, _, Just filePath) -> (Just . StartLua) filePath
          (Just actionName, _, _, _) -> Start <$> parseActionName actionName
          (_, Just actionName, _, _) -> Stop <$> parseActionName actionName
          (_, _, Just actionName, _) -> do
            sendToAction <- parseActionName actionName
            SendTo sendToAction <$> msg
          _ -> Nothing
