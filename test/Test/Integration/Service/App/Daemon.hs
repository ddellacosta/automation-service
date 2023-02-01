module Test.Integration.Service.App.Daemon
  ( spec_
  ,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as M
import Test.Hspec (Spec, before, describe, expectationFailure, it, shouldBe)
import Test.Integration.TestApp (initEnv, testActionsService)
import Service.App.Daemon (DeviceMap, ThreadMap, initializeAndRunAction)
import Service.ActionName (ActionName(..))
import UnliftIO.STM (newBroadcastTChanIO, newTVarIO, readTVarIO)

spec_ :: Spec
spec_ = describe "Service.App.Daemon specs" $ do
  threadMapSpecs

threadMapSpecs :: Spec
threadMapSpecs =
  before initEnv $ do
    describe "ThreadMap Specs" $ do 
      it "adds an entry to the ThreadMap List indexed by ActionName" $ \env -> do
        broadcastChan <- newBroadcastTChanIO
        threadMap <- liftIO $ newTVarIO (M.empty :: (Monad m) => ThreadMap m)
        deviceMap <- liftIO $ newTVarIO (M.empty :: DeviceMap)

        testActionsService env $
          initializeAndRunAction threadMap deviceMap broadcastChan Test

        threadMap' <- readTVarIO threadMap
        case M.lookup Test threadMap' of
          Just testActions -> length testActions `shouldBe` 1
          Nothing -> expectationFailure "Should have an action at index ActionName `Test`"


{-

* create TestAction scaffolding that will work with the Daemon infrastructure and can communicate back and forth, hold stateful values for test validation, etc.

* test that ThreadMap and DeviceMap both have the action async added, to the right place
* test that ThreadMap and DeviceMap both have the action async removed, from the right place

-}
