module Test.AutomationService.Spec where

import Effect (Effect)
import Effect.Aff (Aff)
import Data.Unit (Unit)
import Test.Spec (SpecT)

type Spec a = SpecT Aff Unit Effect a
