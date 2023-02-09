module Test.Integration.Service.App.Daemon
  ( spec_
  ,
  )
where

import Control.Lens ((^.))
import qualified Data.Map.Strict as M
import Service.App.Daemon (initializeAndRunAction)
import Service.App.DaemonState (DaemonState(_threadMap))
import Service.ActionName (ActionName(..))
import Service.Env (config)
import Test.Hspec (Spec, aroundAll, describe, expectationFailure, it, shouldBe, pending)
import Test.Integration.TestApp (testActionsService)
import Test.Integration.Service.App.DaemonTestHelpers (findAction, initAndCleanup)
import UnliftIO.STM (readTVarIO)

spec_ :: Spec
spec_ = describe "Service.App.Daemon specs" $ do
  threadMapSpecs

threadMapSpecs :: Spec
threadMapSpecs =
  aroundAll initAndCleanup $ do
    describe "Daemon's stateful behavior" $ do
      it "adds an entry to the ThreadMap List indexed by ActionName" $
        \(env, daemonState) -> do
          testActionsService env $ initializeAndRunAction daemonState Test findAction
          threadMap' <- readTVarIO (_threadMap daemonState)
          case M.lookup Test threadMap' of
            Just testActions -> length testActions `shouldBe` 1
            Nothing ->
              expectationFailure "Should have an action at index ActionName `Test`"

      it "removes conflicting Device entries from ThreadMap for given ActionName" $
        \(env, daemonState) -> do
          pending
          putStrLn $ show (env ^. config)
          testActionsService env $ initializeAndRunAction daemonState Test findAction
          threadMap' <- readTVarIO (_threadMap daemonState)
          case M.lookup Test threadMap' of
            Just testActions -> length testActions `shouldBe` 1
            Nothing ->
              expectationFailure "Should have an action at index ActionName `Test`"


{-

* create TestAction scaffolding that will work with the Daemon infrastructure and can communicate back and forth, hold stateful values for test validation, etc.

* test that ThreadMap and DeviceMap both have the action async added, to the right place
* test that ThreadMap and DeviceMap both have the action async removed, from the right place

-}
