{-# LANGUAGE TemplateHaskell #-}

module Service.Device
  ( DeviceId(..)
  , Device(..)
  , category
  , id
  , name
  )
where

import Prelude hiding (id)

import Control.Lens ((^?), folded, filtered, makeFieldsNoPrefix)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)

type DeviceId = Text

data Device = Device
  { _id :: DeviceId
  , _name :: Text
  , _category :: Text
  }
  deriving (Generic, Show, Eq)

makeFieldsNoPrefix ''Device
