{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Service.Device
  ( DeviceId
  , Device(..)
  , category
  , id
  , manufacturer
  , model
  , name
  )
where

import Prelude hiding (id)

import Control.Lens (makeFieldsNoPrefix)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

type DeviceId = Text

data Device = Device
  { _id :: DeviceId
  , _name :: Text
  , _category :: Text
  , _manufacturer :: Maybe Text
  , _model :: Maybe Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

makeFieldsNoPrefix ''Device
