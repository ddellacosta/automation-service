module Service.MQTT.Messages.Lighting
  ( Color(..)
  , ColorXY(..)
  , Effect(..)
  , EffectMsg(..)
  , Hex(..)
  , RGB(..)
  , Transition(..)
  , colorXY
  , effect'
  , hex'
  , mkColorXY
  , mkEffectMsg
  , mkHex
  , mkRGB
  , seconds
  , withTransition
  , withTransition'
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), decode, defaultOptions, encode,
                   genericToEncoding)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)

data Color a = Color
  { color :: a
  }
  deriving (Generic, Show, Eq, Ord)

instance ToJSON a => ToJSON (Color a) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON a => FromJSON (Color a)

-- Effect enum and Effect message helper

data Effect =
  Blink | Breathe | Okay | ChannelChange | FinishEffect | StopEffect
  deriving (Generic, Show, Eq, Ord)

effectToJson :: Effect -> Text
effectToJson = \case
  Blink         -> "blink"
  Breathe       -> "breathe"
  Okay          -> "okay"
  ChannelChange -> "channel_change"
  FinishEffect  -> "finish_effect"
  StopEffect    -> "stop_effect"

data EffectMsg = EffectMsg
  { effect :: Text
  }
  deriving (Generic, Show, Eq, Ord)

instance ToJSON EffectMsg where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON EffectMsg

mkEffectMsg :: Effect -> EffectMsg
mkEffectMsg = EffectMsg . effectToJson


-- CIE 1931 color space

data ColorXY = ColorXY
  { x :: Double
  , y :: Double
  }
  deriving (Generic, Show, Eq, Ord)

instance ToJSON ColorXY where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ColorXY

mkColorXY :: Double -> Double -> Color ColorXY
mkColorXY x y = Color $ ColorXY x y


-- Hex format for color changes

data Hex = Hex
  { hex :: Text
  }
  deriving (Generic, Show, Eq, Ord)

instance ToJSON Hex where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Hex

mkHex :: Text -> Color Hex
mkHex = Color . Hex


-- RGB format for color changes

data RGB = RGB
  { r :: Int
  , g :: Int
  , b :: Int
  }
  deriving (Generic, Show, Eq, Ord)

instance ToJSON RGB where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON RGB

mkRGB :: Int -> Int -> Int -> Color RGB
mkRGB r g b = Color $ RGB r g b


-- passed with other messages to indicate a duration of time it should
-- take for the message's intended action to take effect

data Transition = Transition
  { transition :: Int
  }
  deriving (Generic, Show, Eq, Ord)

instance ToJSON Transition where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Transition

withTransition :: (ToJSON msg, FromJSON msg) => Int -> msg -> Value
withTransition seconds' msg =
  maybe Null (\v -> mergeObjects v $ transitionValue seconds') (decode (encode msg))
  where
    mergeObjects obj1 obj2 =
      case (obj1, obj2) of
        ((Object obj1'), (Object obj2')) -> Object $ obj1' <> obj2'
        (_, _)                           -> Null

    transitionValue :: Int -> Value
    transitionValue s = fromMaybe Null $ decode $ encode $ Transition s


-- simple helpers

-- this probably doesn't belong in here, but I'm using it in all the
-- same places now, so meh
seconds :: Int -> Int
seconds n = n * 10000000

effect' :: Effect -> ByteString
effect' = encode . mkEffectMsg

hex' :: Text -> ByteString
hex' = encode . mkHex

colorXY :: Double -> Double -> ByteString
colorXY x y = encode $ mkColorXY x y

withTransition' :: (ToJSON msg, FromJSON msg) => Int -> msg -> ByteString
withTransition' s msg = encode $ withTransition s msg
