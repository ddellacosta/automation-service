module Test.Unit.Service.App.ThreadMap
  ( spec
  )
where

import Control.Lens ((<&>), _head, preview)
import qualified Data.Map.Strict as M
import Test.Hspec (Spec, describe, it, shouldBe)
import Service.Action (Action, ActionFor(name), nullAction)
import Service.ActionName (ActionName(Null))
import Service.App.ThreadMap (ThreadMap, insertAction)
import UnliftIO.Async (Async, async)
import UnliftIO.STM (atomically, newTVarIO, readTVarIO)

actionEntryFixture :: IO (Action IO, Async ()) 
actionEntryFixture = do
  entryAsync <- async $ pure ()
  pure (nullAction, entryAsync)

spec :: Spec
spec = describe "Tests ThreadMap functions" $ do
  it "inserts a new Action pair into a ThreadMap" $ do
    let actionName = Null
    threadMap <- newTVarIO (M.empty :: ThreadMap IO)
    actionEntry <- actionEntryFixture
    atomically $ insertAction threadMap actionName actionEntry
    threadMap' <- readTVarIO threadMap

    let
      newActionEntries = M.lookup actionName threadMap'
      newActionEntry = newActionEntries >>= preview _head

    (newActionEntries <&> length) `shouldBe` Just 1
    (newActionEntry <&> fst <&> name) `shouldBe` Just actionName
