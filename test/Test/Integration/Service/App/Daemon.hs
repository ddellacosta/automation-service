module Test.Integration.Service.App.Daemon
  ( spec_
  ,
  )
where

import qualified Data.Map.Strict as M
import Service.App.Daemon (initializeAndRunAction)
import Service.App.DaemonState (DaemonState(_threadMap))
import Service.ActionName (ActionName(..))
import Test.Hspec (Spec, aroundAll, describe, expectationFailure, it, shouldBe)
import Test.Integration.TestApp (testActionsService)
import Test.Integration.Service.App.DaemonTestHelpers (findAction, initAndCleanup)
import UnliftIO.STM (readTVarIO)

spec_ :: Spec
spec_ = describe "Service.App.Daemon specs" $ do
  threadMapSpecs

threadMapSpecs :: Spec
threadMapSpecs =
  -- These tests are stateful--see the aroundAll wrapper--and
  -- therefore the order they are run has an effect on the behavior of
  -- any given test. Be careful and create a new Spec if you want to
  -- reset all state every time.
  aroundAll initAndCleanup $ do
    describe "Daemon's stateful behavior" $ do
      it "adds an entry to the ThreadMap List indexed by ActionName" $
        \(env, daemonState) -> do
          testActionsService env $ initializeAndRunAction daemonState Test findAction
          threadMap' <- readTVarIO (_threadMap daemonState)
          lookupOrFail "Should have an action at index ActionName `Test`" threadMap' $
            \testActions -> length testActions `shouldBe` 1

      -- TODO 3 out of 17 runs of this failed for reasons I don't
      -- understand--this is a flaky test. We have a race condition
      it "removes conflicting Device entries from ThreadMap for given ActionName" $
        \(env, daemonState) -> do
          testActionsService env $ initializeAndRunAction daemonState Test findAction
          threadMap' <- readTVarIO (_threadMap daemonState)
          lookupOrFail "Should have an action at index ActionName `Test`" threadMap' $
            \testActions -> length testActions `shouldBe` 1

  where
    lookupOrFail msg threadMap' assertion =
      case M.lookup Test threadMap' of
        Just testActions -> assertion testActions
        Nothing -> expectationFailure msg

{-

* create TestAction scaffolding that will work with the Daemon infrastructure and can communicate back and forth, hold stateful values for test validation, etc.

* test that ThreadMap and DeviceMap both have the action async added, to the right place
* test that ThreadMap and DeviceMap both have the action async removed, from the right place

-}
