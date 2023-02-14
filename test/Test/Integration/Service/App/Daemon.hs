module Test.Integration.Service.App.Daemon
  ( spec
  ,
  )
where

import Control.Lens ((^.))
import Control.Monad (void)
import qualified Data.Map.Strict as M
import qualified Service.App.Daemon as Daemon
import Service.App.DaemonState
  ( DaemonState(_deviceMap, _threadMap)
  , ServerResponse
  , initDaemonState
  )
import Service.ActionName (ActionName(..))
import Service.Device (DeviceId(..))
import Service.Env (messageQueue)
import qualified Service.Messages.Action as Messages
import Test.Hspec (Expectation, Spec, around, expectationFailure, it, shouldBe)
import Test.Integration.TestApp (Env, TestActionsService, testActionsService)
import Test.Integration.Service.App.DaemonTestHelpers
  ( findAction
  , initAndCleanup
  )
import UnliftIO.STM (TQueue, atomically, newTQueueIO, readTQueue, readTVarIO, writeTQueue)
import UnliftIO.Async (withAsync)

spec :: Spec
spec = do
  threadMapSpecs
  deviceMapSpecs

threadMapSpecs :: Spec
threadMapSpecs = do
  around initAndCleanup $ do
    it "adds an entry to the ThreadMap List indexed by ActionName" $
      testWithAsyncDaemon $ \daemonState messageQueue' responseQueue -> do
        atomically $ writeTQueue messageQueue' $ Messages.Start Test
        blockUntilNextEventLoop responseQueue
        threadMap' <- readTVarIO $ _threadMap daemonState
        lookupOrFail "Should have an action at index ActionName `Test`" Test threadMap' $
          \testActions -> length testActions `shouldBe` 1

  around initAndCleanup $ do
    it "removes conflicting Action entries using 'owned' Devices from ThreadMap when starting" $
      testWithAsyncDaemon $ \daemonState messageQueue' responseQueue -> do
        atomically $ writeTQueue messageQueue' $ Messages.Start Test
        blockUntilNextEventLoop responseQueue
        atomically $ writeTQueue messageQueue' $ Messages.Start Test
        blockUntilNextEventLoop responseQueue
        threadMap' <- readTVarIO $ _threadMap daemonState
        lookupOrFail "Should have an action at index ActionName `Test`" Test threadMap' $
          \testActions -> length testActions `shouldBe` 1

  around initAndCleanup $ do
    it "removes entries from ThreadMap when stopping" $
      testWithAsyncDaemon $ \daemonState messageQueue' responseQueue -> do
        atomically $ writeTQueue messageQueue' $ Messages.Start Test
        blockUntilNextEventLoop responseQueue
        atomically $ writeTQueue messageQueue' $ Messages.Stop Test
        blockUntilNextEventLoop responseQueue
        threadMap' <- readTVarIO $ _threadMap daemonState
        -- the void hack here is because there is no Show
        -- instance for Just Action, but there is one for Just (), and
        -- all I care about with this test is the effect, not the
        -- value
        (void . M.lookup Test) threadMap' `shouldBe` Nothing

deviceMapSpecs :: Spec
deviceMapSpecs = do
  around initAndCleanup $ do
    it "adds an entry to the DeviceMap List indexed by DeviceId" $
      testWithAsyncDaemon $ \daemonState messageQueue' responseQueue -> do
        atomically $ writeTQueue messageQueue' $ Messages.Start Test
        blockUntilNextEventLoop responseQueue
        deviceMap' <- readTVarIO $ _deviceMap daemonState
        lookupOrFail "Should have an action at index ActionName `Test`" TestDevice deviceMap'
          (\testActionNames -> length testActionNames `shouldBe` 1)

    it "removes ActionName from the DeviceMap entry when the Action using it is shut down" $
      testWithAsyncDaemon $ \daemonState messageQueue' responseQueue -> do
        atomically $ writeTQueue messageQueue' $ Messages.Start Test
        blockUntilNextEventLoop responseQueue
        atomically $ writeTQueue messageQueue' $ Messages.Stop Test
        blockUntilNextEventLoop responseQueue
        deviceMap' <- readTVarIO $ _deviceMap daemonState
        -- see comments above about void hack
        (void . M.lookup TestDevice) deviceMap' `shouldBe` Nothing

    it "ensures proper bookkeeping for DeviceMap entries when an Action is shut down due to another Action starting" $
      testWithAsyncDaemon $ \daemonState messageQueue' responseQueue -> do
        atomically $ writeTQueue messageQueue' $ Messages.Start Test
        blockUntilNextEventLoop responseQueue
        atomically $ writeTQueue messageQueue' $ Messages.Start Test
        blockUntilNextEventLoop responseQueue
        deviceMap' <- readTVarIO $ _deviceMap daemonState
        lookupOrFail "Should have an action at index ActionName `Test`" TestDevice deviceMap' $
          \testActionNames -> length testActionNames `shouldBe` 1

--
-- helper utils
--

blockUntilNextEventLoop :: TQueue ServerResponse -> IO ()
blockUntilNextEventLoop = void . atomically . readTQueue

testWithAsyncDaemon
  ::
    (  DaemonState TestActionsService
    -> TQueue Messages.Action
    -> TQueue ServerResponse
    -> Expectation
    )
  -> Env
  -> Expectation
testWithAsyncDaemon test env = do
  let messageQueue' = env ^. messageQueue
  responseQueue <- newTQueueIO
  daemonState <- initDaemonState
  withAsync (testActionsService env $ Daemon.run' daemonState findAction responseQueue) $
    \_async -> test daemonState messageQueue' responseQueue

lookupOrFail :: (Ord k) => String -> k -> M.Map k v -> (v -> Expectation) -> Expectation
lookupOrFail msg k m assertion =
  case M.lookup k m of
    Just v -> assertion v
    Nothing -> expectationFailure msg
