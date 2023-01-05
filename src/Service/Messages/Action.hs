module Service.Messages.Action
  ( Action(..)
  ,
  )
where


import Control.Lens ((??))
import qualified Data.Aeson as Aeson.Value
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value
  , defaultOptions
  , genericToEncoding
  , withObject
  )
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Service.ActionName (ActionName, parseActionName)

data Action msg =
  Start ActionName |
  Stop ActionName |
  SendTo ActionName msg |
  Null
  deriving (Generic, Eq, Ord, Show)

--
-- TODO generalize this for different msg types
--
instance ToJSON (Action Text) where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON (Action Text) where
  parseJSON = withObject "Action" $ \o ->
    pure $ fromMaybe Null (toAction o)
    where
      toText v = case v of
        Aeson.Value.String t -> t
        _ -> ""

      toAction :: KM.KeyMap Value -> Maybe (Action Text)
      toAction o
        | KM.member "start" o = do
            actionName' <- KM.lookup "start" o
            Start <$> (parseActionName $ toText actionName')
        | KM.member "stop" o = do
            actionName <- KM.lookup "stop" o
            Stop <$> (parseActionName $ toText actionName)
        | KM.member "send" o = do
            actionName <- KM.lookup "send" o
            msg <- KM.lookup "msg" o
            (SendTo <$> (parseActionName $ toText actionName) ?? toText msg)
        | otherwise = Nothing
