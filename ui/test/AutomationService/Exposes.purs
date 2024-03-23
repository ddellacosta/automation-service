module Test.AutomationService.Exposes where

import Debug (trace, traceM)

import AutomationService.Exposes (Exposes, decodeExposes)
import Data.Argonaut (Json, JsonDecodeError)
import Data.Argonaut.Core (toArray)
import Data.Argonaut.Decode ((.:), (.:?), fromJsonString, decodeJson)
import Data.Either (Either, fromRight)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for, for_)
import Prelude (Unit, ($), (>>=), (<$>), (<#>), bind, const, discard, flip, map, pure)
import Test.Fixtures (signeFixture)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- import Data.Argonaut.Core (stringify)
-- import Data.Map as M
-- import Effect (Effect)
-- import Effect.Aff (Aff, launchAff_)
-- import Effect.Class (liftEffect)
-- import Effect.Console (log)
-- import Effect.Ref (Ref)
-- import Effect.Ref as Ref
-- import Elmish.Component (Command)
-- import Elmish.Test (find, prop, testComponent, text, (>>))
-- import Elmish.Test.DomProps as P
-- import Elmish.Test.Events (change, click)
-- import Main as Main
-- import Test.Spec (Spec, before, describe, it)
-- import Test.Spec.Assertions (shouldEqual)
-- import Test.Spec.Reporter (consoleReporter)
-- import Test.Spec.Runner (runSpec)


spec :: Spec Unit
spec = -- before setup $
  describe "Main app" $
    it "Can do the thing" $ \wsState -> do

      traceM signeFixture
      let
        signe = do
          obj <- fromJsonString signeFixture
          definition <- obj .: "definition"
          exposes' <- definition .:? "exposes" :: Either JsonDecodeError (Maybe (Array Json))
          traceM exposes'
          for (fromMaybe [] exposes') $ \e -> do
            obj <- decodeJson e
            name :: Maybe String <- obj .:? "name"
            for name $ const $ decodeExposes Nothing e

        signe' = fromRight [] signe

      for_ signe' $ traceM

      --- traceM signe

      "Hey" `shouldEqual` "No"



--   where
     -- setup :: Aff (Ref String)
     -- setup = liftEffect $ Ref.new "foo"

     -- withTestId :: String -> String -> String
     -- withTestId sel testId = sel <> "[data-test-id=\"" <> testId <> "\"]"
