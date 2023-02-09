module Test.Integration.Service.App.Daemon
  ( spec_
  ,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as M
import Test.Hspec (Spec, before, describe, expectationFailure, it, shouldBe)
import Test.Integration.TestApp (initEnv, testActionsService)
import Service.App.Daemon (initializeAndRunAction)
import Service.App.DaemonState (DaemonState(..), DeviceMap, ThreadMap)
import Service.ActionName (ActionName(..))
import UnliftIO.STM (atomically, dupTChan, newBroadcastTChanIO, newTVarIO, readTVarIO)

spec_ :: Spec
spec_ = describe "Service.App.Daemon specs" $ do
  threadMapSpecs

threadMapSpecs :: Spec
threadMapSpecs =
  before initEnv $ do
    describe "ThreadMap Specs" $ do 
      it "adds an entry to the ThreadMap List indexed by ActionName" $ \env -> do
        broadcastChan <- newBroadcastTChanIO
        serverChan <- atomically $ dupTChan broadcastChan
        threadMap <- liftIO $ newTVarIO (M.empty :: ThreadMap m)
        deviceMap <- liftIO $ newTVarIO (M.empty :: DeviceMap)
        let daemonState = DaemonState threadMap deviceMap broadcastChan serverChan

        testActionsService env $
          initializeAndRunAction daemonState Test

        threadMap' <- readTVarIO threadMap
        case M.lookup Test threadMap' of
          Just testActions -> length testActions `shouldBe` 1
          Nothing -> expectationFailure "Should have an action at index ActionName `Test`"


{-

* create TestAction scaffolding that will work with the Daemon infrastructure and can communicate back and forth, hold stateful values for test validation, etc.

* test that ThreadMap and DeviceMap both have the action async added, to the right place
* test that ThreadMap and DeviceMap both have the action async removed, from the right place

-}
