{-# LANGUAGE TemplateHaskell #-}

module Service.Adapters.Group
  ( Endpoint(..)
  , Group(..)
  , GroupDevices
  , GroupId
  , Groups(..)
  , id
  , devices
  , name
  )
where

import Control.Lens (makeFieldsNoPrefix)
import Data.Aeson (FromJSON, ToJSON, Value(..), (.:), defaultOptions, genericToEncoding, parseJSON, toEncoding, withArray, withObject)
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Prelude (Eq, Int, Show, ($), (<$>), (=<<), (/=), (&&), pure)
import Service.Adapters.Device (DeviceId)


newtype Endpoint = Endpoint Int
  deriving (Eq, Generic, Show)

instance ToJSON Endpoint where
  toEncoding = genericToEncoding defaultOptions

type GroupDevices = M.HashMap DeviceId Endpoint

type GroupId = Int

data Group = Group
  { _id :: GroupId
  , _name :: T.Text
  , _devices :: GroupDevices
  } deriving (Eq, Generic, Show)

makeFieldsNoPrefix ''Group

decodeDevices :: Value -> Parser GroupDevices
decodeDevices (Array gds) =
   V.foldM
     (\gdM -> \case
         (Object gd) -> do
           ieeeAddress' <- gd .: "ieee_address"
           endpoint' <- gd .: "endpoint"
           let gd' = Endpoint endpoint'
           pure $ M.insert ieeeAddress' gd' gdM

         _ ->
           pure gdM
     )
     M.empty
     gds
decodeDevices _ = pure M.empty

instance FromJSON Group where
  parseJSON = withObject "Group" $ \g -> do
    gName <- g .: "friendly_name"
    gId <- g .: "id"
    gDevices <- decodeDevices =<< g .: "members"
    pure $ Group gId gName gDevices

instance ToJSON Group where
  toEncoding = genericToEncoding defaultOptions

-- Same as with Devices, a convenience type to allow me to easily call
-- `decode` on the message I get back from zigbee2mqtt/bridge/groups
-- initially
data Groups = Groups
  { loadGroups :: M.HashMap GroupId Group
  } deriving (Eq, Generic, Show)

instance FromJSON Groups where
 parseJSON = withArray "Groups" $ \a ->
    Groups <$>
      V.foldM
        (\gM g -> do
            group <- parseJSON g
            -- will break this out further into a set of filters at
            -- the top level that is applied generally when parsing
            -- groups without the parser needing to have special
            -- knowledge
            if (_id group /= 901 && _name group /= "default_bind_group")
            then
              pure $ M.insert (_id group) group gM
            else
              pure gM
        )
        M.empty
        a
