module Test.AutomationService.Exposes where

import Debug (trace, traceM)

import AutomationService.Exposes (Exposes, decodeExposes)
import Data.Argonaut (Json, JsonDecodeError)
import Data.Argonaut.Core (toArray)
import Data.Argonaut.Decode ((.:), (.:?), fromJsonString, decodeJson)
import Data.Array (catMaybes)
import Data.Either (Either, fromRight)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for, for_)
import Prelude (Unit, ($), (>>=), (<$>), (<#>), (<>), bind, const, discard, flip, identity, map, pure, show)
import Test.Fixtures (signeFixture)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)


spec :: Spec Unit
spec =
  describe "Main app" $
    it "Can do the thing" $ \wsState -> do

      let
        signe :: Either JsonDecodeError (Array Exposes)
        signe = do
          obj <- fromJsonString signeFixture
          definition <- obj .: "definition"
          exposes' <- definition .:? "exposes" :: Either JsonDecodeError (Maybe (Array Json))
          decodeExposes Nothing (fromMaybe [] exposes')

        signe' = fromRight [] signe

      traceM signe
      for_ signe' $ \e -> traceM (show e)

      "Hey" `shouldEqual` "No"
