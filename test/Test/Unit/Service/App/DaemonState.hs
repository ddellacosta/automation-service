module Test.Unit.Service.App.DaemonState
  ( spec
  )
where

import Control.Lens ((^?), _1, ix)
import qualified Data.Map.Strict as M
import Test.Hspec (Spec, describe, it, shouldBe)
import Service.Automation (Automation, name, nullAutomation)
import Service.AutomationName (AutomationName(Null))
import Service.App.DaemonState (ThreadMap, insertAutomation)
import UnliftIO.Async (Async, async)
import UnliftIO.STM (atomically, newTVarIO, readTVarIO)

actionEntryFixture :: IO (Automation IO, Async ())
actionEntryFixture = do
  entryAsync <- async $ pure ()
  pure (nullAutomation, entryAsync)

spec :: Spec
spec = describe "Tests ThreadMap functions" $ do
  it "inserts a new Automation pair into a ThreadMap" $ do
    let actionName = Null
    threadMap <- newTVarIO (M.empty :: ThreadMap IO)
    actionEntry <- actionEntryFixture
    atomically $ insertAutomation threadMap actionName actionEntry
    threadMap' <- readTVarIO threadMap

    let
      newAutomationName = threadMap' ^? ix actionName . _1 . name

    newAutomationName `shouldBe` Just actionName
