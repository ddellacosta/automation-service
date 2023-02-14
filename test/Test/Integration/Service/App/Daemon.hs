module Test.Integration.Service.App.Daemon
  ( spec
  ,
  )
where

import Control.Monad (void)
import qualified Data.Map.Strict as M
import Service.App.DaemonState (DaemonState(_deviceMap, _threadMap))
import Service.ActionName (ActionName(..))
import qualified Service.Device as Device
import qualified Service.Messages.Action as Messages
import Test.Hspec (Spec, around, it, shouldBe)
import Test.Integration.Service.App.DaemonTestHelpers
  ( blockUntilNextEventLoop
  , initAndCleanup
  , lookupOrFail
  , testWithAsyncDaemon
  )
import UnliftIO.STM (atomically, readTVarIO, writeTQueue)

spec :: Spec
spec = do
  threadMapSpecs
  deviceMapSpecs

threadMapSpecs :: Spec
threadMapSpecs = do
  around initAndCleanup $ do
    it "adds an entry to the ThreadMap List indexed by ActionName" $
      testWithAsyncDaemon $ \daemonState messageQueue' responseQueue -> do
        atomically $ writeTQueue messageQueue' $ Messages.Start Gold
        blockUntilNextEventLoop responseQueue
        threadMap' <- readTVarIO $ _threadMap daemonState
        lookupOrFail "Should have an action at index ActionName `Gold`" Gold threadMap' $
          \testActions -> length testActions `shouldBe` 1

  around initAndCleanup $ do
    it "removes conflicting Action entries using 'owned' Devices from ThreadMap when starting" $
      testWithAsyncDaemon $ \daemonState messageQueue' responseQueue -> do
        atomically $ writeTQueue messageQueue' $ Messages.Start Gold
        blockUntilNextEventLoop responseQueue
        atomically $ writeTQueue messageQueue' $ Messages.Start Gold
        blockUntilNextEventLoop responseQueue
        threadMap' <- readTVarIO $ _threadMap daemonState
        lookupOrFail "Should have an action at index ActionName `Gold`" Gold threadMap' $
          \testActions -> length testActions `shouldBe` 1

  around initAndCleanup $ do
    it "removes entries from ThreadMap when stopping" $
      testWithAsyncDaemon $ \daemonState messageQueue' responseQueue -> do
        atomically $ writeTQueue messageQueue' $ Messages.Start Gold
        blockUntilNextEventLoop responseQueue
        atomically $ writeTQueue messageQueue' $ Messages.Stop Gold
        blockUntilNextEventLoop responseQueue
        threadMap' <- readTVarIO $ _threadMap daemonState
        -- the void hack here is because there is no Show
        -- instance for Just Action, but there is one for Just (), and
        -- all I care about with this test is the effect, not the
        -- value
        (void . M.lookup Gold) threadMap' `shouldBe` Nothing

deviceMapSpecs :: Spec
deviceMapSpecs = do
  around initAndCleanup $ do
    it "adds an entry to the DeviceMap List indexed by DeviceId" $
      testWithAsyncDaemon $ \daemonState messageQueue' responseQueue -> do
        atomically $ writeTQueue messageQueue' $ Messages.Start Gold
        blockUntilNextEventLoop responseQueue
        deviceMap' <- readTVarIO $ _deviceMap daemonState
        lookupOrFail "Should have an action at index ActionName `Gold`" Device.GledoptoGLC007P_1 deviceMap'
          (\testActionNames -> length testActionNames `shouldBe` 1)

    it "removes ActionName from the DeviceMap entry when the Action using it is shut down" $
      testWithAsyncDaemon $ \daemonState messageQueue' responseQueue -> do
        atomically $ writeTQueue messageQueue' $ Messages.Start Gold
        blockUntilNextEventLoop responseQueue
        atomically $ writeTQueue messageQueue' $ Messages.Stop Gold
        blockUntilNextEventLoop responseQueue
        deviceMap' <- readTVarIO $ _deviceMap daemonState
        -- see comments above about void hack
        (void . M.lookup Device.GledoptoGLC007P_1) deviceMap' `shouldBe` Nothing

    it "ensures proper bookkeeping for DeviceMap entries when an Action is shut down due to another Action starting" $
      testWithAsyncDaemon $ \daemonState messageQueue' responseQueue -> do
        atomically $ writeTQueue messageQueue' $ Messages.Start Gold
        blockUntilNextEventLoop responseQueue
        atomically $ writeTQueue messageQueue' $ Messages.Start Gold
        blockUntilNextEventLoop responseQueue
        deviceMap' <- readTVarIO $ _deviceMap daemonState
        lookupOrFail "Should have an action at index ActionName `Gold`" Device.GledoptoGLC007P_1 deviceMap' $
          \testActionNames -> length testActionNames `shouldBe` 1
