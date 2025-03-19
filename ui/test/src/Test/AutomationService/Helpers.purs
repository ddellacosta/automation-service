module Test.AutomationService.Helpers
  ( construct
  , shouldConstruct
  , shouldHaveCapabilities
  )
where

import AutomationService.Device (Device, DeviceDetails, details)
import AutomationService.Exposes (CapType(..), Capability, CapabilityDetails, Exposes, SubProps(..), matchingCapabilities)
import Control.Monad (when)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array (length)
import Data.Eq (class Eq)
import Data.Functor (class Functor)
import Data.Generic.Rep (Constructor(..), Sum(..))
import Data.Generic.Rep as Generic
import Data.Maybe (Maybe(..))
import Data.Show (class Show)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (class Traversable, for_)
import Effect.Exception (Error, error)
import Prelude (Unit, ($), (<<<), (<$>), (<#>), (<>), (==), flip, not, show)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

-- |
-- | shouldConstruct is a bit of a hack to allow testing
-- | of devices inside of an (Show-able, Eq-able) Functorial
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
  => Functor t
  => (DeviceDetails -> Device)
  -> t Device
  -> m Unit
shouldConstruct cons v = construct cons v `shouldEqual` v

construct
  :: forall t. Functor t
  => (DeviceDetails -> Device)
  -> t Device
  -> t Device
construct cons v = cons <<< details <$> v


-- |
-- | Slightly over-the-top helper for producing good failure messages
-- | when supplying a test using an array of Capability constructors
-- | to compare against an Exposes object (a.k.a. an array of Capbility
-- | values), a la
-- |
-- |     light `shouldHaveCapabilities` [OnOff, Occupancy]
-- |
-- |     -- ...produces test run failure:
-- |
-- |     1) Exposes
-- |          Can parse a collection of Exposes JSON:
-- |        Error: Does not have capabilities ["OnOff","Occupancy"], has only (Right ["OnOff"])
-- |
shouldHaveCapabilities
  :: forall m t. MonadThrow Error m
  => Traversable t
  => Show (t (Array String))
  => t Exposes
  -> Array (CapabilityDetails -> Capability)
  -> m Unit
shouldHaveCapabilities exposes caps =
  for_ isMatch \isMatch' ->
    when (not isMatch') do
      let
        capNames = show $ capName <$> caps
        -- lol
        matchNames = show $ matches <#> \matches' -> capName <$> matches'
      throwError <<< error $
        "Does not have capabilities " <> capNames <> ", has only " <> matchNames

  where
    matches = flip matchingCapabilities caps <$> exposes
    isMatch = matches <#> \ms -> length ms == length caps

    capName cons = capabilityName <<< cons $ dummy

    dummy :: CapabilityDetails
    dummy =
      { type: UnknownCT "dummy"
      , featureType: Nothing
      , name: "dummy"
      , description: Nothing
      , label: "dummy"
      , property: Nothing
      , access: 0
      , subProps: Null
      }

--
-- CapabilityName is a stupid Generic-based helper to get the
-- Constructor name as a string. It's highly coupled to the structure
-- of the Capability type's Generic representation, so it's a
-- Generic-based type class in name only.
--
class CapabilityName a where
  capName :: a -> String

instance (CapabilityName a, CapabilityName b) => CapabilityName (Sum a b) where
  capName = case _ of
    Inl a -> capName a
    Inr b -> capName b

instance (IsSymbol name) => CapabilityName (Constructor name a) where
  capName (Constructor _a) =
    reflectSymbol (Proxy :: Proxy name)

capabilityName :: Capability -> String
capabilityName = capName <<< Generic.from
