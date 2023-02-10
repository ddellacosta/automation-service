module Test.Integration.Service.App.Daemon
  ( spec
  ,
  )
where

import Control.Lens ((^.))
import qualified Data.Map.Strict as M
import qualified Service.App.Daemon as Daemon
import Service.App.DaemonState
  ( ActionEntry
  , DaemonState(_threadMap)
  , ThreadMap
  , initDaemonState
  )
import Service.ActionName (ActionName(..))
import Service.Env (messagesQueue)
import qualified Service.Messages.Action as Messages
import Test.Hspec (Expectation, Spec, around, describe, expectationFailure, it, shouldBe)
import Test.Integration.TestApp (testActionsService)
import Test.Integration.Service.App.DaemonTestHelpers
  ( findAction
  , initAndCleanup
  )
import UnliftIO.STM (atomically, newTQueueIO, readTQueue, readTVarIO, writeTQueue)
import UnliftIO.Async (withAsync)

spec :: Spec
spec = describe "Service.App.Daemon specs" $ do
  threadMapSpecs

threadMapSpecs :: Spec
threadMapSpecs = do
  around initAndCleanup $ do
    it "adds an entry to the ThreadMap List indexed by ActionName" $
      \env -> do
        let messagesQueue' = env ^. messagesQueue
        responseQueue <- newTQueueIO
        daemonState <- initDaemonState
        withAsync (testActionsService env $ Daemon.run' daemonState findAction responseQueue) $
          \_async -> do
            atomically $ writeTQueue messagesQueue' $ Messages.Start Test
            -- block until we get to end of event loop
            _ <- atomically $ readTQueue responseQueue
            threadMap' <- readTVarIO $ _threadMap daemonState
            lookupOrFail "Should have an action at index ActionName `Test`" threadMap' $
              \testActions -> length testActions `shouldBe` 1

  around initAndCleanup $ do
    it "removes conflicting Action entries using 'owned' Devices from ThreadMap when starting" $
      \env -> do
        let messagesQueue' = env ^. messagesQueue
        responseQueue <- newTQueueIO
        daemonState <- initDaemonState
        withAsync (testActionsService env $ Daemon.run' daemonState findAction responseQueue) $
          \_async -> do
            atomically $ writeTQueue messagesQueue' $ Messages.Start Test
            -- block until we get to end of event loop
            _ <- atomically $ readTQueue responseQueue
            atomically $ writeTQueue messagesQueue' $ Messages.Start Test
            _ <- atomically $ readTQueue responseQueue
            threadMap' <- readTVarIO $ _threadMap daemonState
            lookupOrFail "Should have an action at index ActionName `Test`" threadMap' $
              \testActions -> length testActions `shouldBe` 1

  where
    lookupOrFail :: String -> ThreadMap m -> ([ActionEntry m] -> Expectation) -> Expectation
    lookupOrFail msg threadMap' assertion =
      case M.lookup Test threadMap' of
        Just testActions -> assertion testActions
        Nothing -> expectationFailure msg

{-

* create TestAction scaffolding that will work with the Daemon infrastructure and can communicate back and forth, hold stateful values for test validation, etc.

* test that ThreadMap and DeviceMap both have the action async added, to the right place
* test that ThreadMap and DeviceMap both have the action async removed, from the right place

-}
