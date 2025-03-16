module Test.AutomationService.Helpers
  ( construct
  , shouldConstruct
  )
where

import AutomationService.Device (Device, DeviceDetails, details)
import Control.Applicative (class Applicative)
import Control.Bind (class Bind)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Eq (class Eq)
import Data.Show (class Show)
import Effect.Exception (Error)
import Prelude (Unit, (<<<), (=<<), pure)
import Test.Spec.Assertions (shouldEqual)

-- |
-- | shouldConstruct is a bit of a hack to allow testing
-- | of devices inside of an (Show-able, Eq-able) Applicative
-- | context directly against a Device data constructor, a la
-- |
-- |     -- Maybe Device
-- |     ExtendedColorlight `shouldConstruct` Just someDevice
-- |
-- |     -- Either JsonDecodeError Device
-- |     OccupancySensor `shouldConstruct` Right someOtherDevice
-- |
-- | Of course you wouldn't deliberately wrap your device
-- | in a context like this, but most of the time it's a
-- | pain to cleanly extract a Device value from a context,
-- | so this makes it easier to test in a way that provides
-- | non-useless (if a bit noisy) failure output when it
-- | fails, e.g.
-- |
-- |     Error:
-- |        (Just (DimmableLight { category: "Router", exposes: (NonEmptyArray [ ... ]) ... } ) )
-- |      ≠ (Just (ExtendedColorLight { category: "Router", exposes: (NonEmptyArray [ ... ]) ... } ) )
-- |
shouldConstruct
  :: forall m t. MonadThrow Error m
  => Show (t Device)
  => Eq (t Device)
  => Applicative t
  => Bind t
  => (DeviceDetails -> Device)
  -> t Device
  -> m Unit
shouldConstruct cons v = construct cons v `shouldEqual` v

construct
  :: forall m. Bind m
  => Applicative m
  => (DeviceDetails -> Device)
  -> m Device
  -> m Device
construct cons v = pure <<< cons <<< details =<< v
