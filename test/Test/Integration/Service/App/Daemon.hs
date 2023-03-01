{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Integration.Service.App.Daemon
  ( spec
  ,
  )
where

import Control.Lens ((^.), (^?), _1)
import Control.Monad (void)
import qualified Data.Map.Strict as M
import Service.App.DaemonState (DaemonState(_deviceMap, _threadMap))
import Service.Automation (name)
import qualified Service.Automations.Gold as Gold
import Service.AutomationName (AutomationName(..))
import Service.Env (LoggerVariant(..), logger)
import qualified Service.Messages.Daemon as Daemon
import Test.Hspec (Spec, around, it, shouldBe)
import Test.Integration.Service.App.DaemonTestHelpers
  ( blockUntilNextEventLoop
  , initAndCleanup
  , lookupOrFail
  , testWithAsyncDaemon
  )
import UnliftIO.STM (atomically, readTQueue, readTVarIO, writeTQueue)

spec :: Spec
spec = do
  luaScriptSpecs
  threadMapSpecs
  deviceMapSpecs
  --
  -- this test is SOOOOOOOPER slow, because the soonest I can
  -- schedule anything is within a minute. I'm probably going to
  -- turn it off and only run it occasionally, gotta figure out a
  -- good build process for that
  --
  -- _schedulerSpecs

-- these fail every once in a while, which makes me think there is
-- some kind of edge case race condition happening here somehow
luaScriptSpecs :: Spec
luaScriptSpecs = do
  around initAndCleanup $ do
    it "starts a Lua script" $
      testWithAsyncDaemon $ \env _daemonState messageQueue' responseQueue -> do
        let (QLogger qLogger) = env ^. logger
        atomically $ writeTQueue messageQueue' $ Daemon.Start (LuaScript "test.lua")
        blockUntilNextEventLoop responseQueue
        lastLog <- atomically . readTQueue $ qLogger
        lastLog `shouldBe` "test.lua: loopAutomation"

    it "shuts down a Lua script" $
      testWithAsyncDaemon $ \env _daemonState messageQueue' responseQueue -> do
        let (QLogger qLogger) = env ^. logger
        atomically $ writeTQueue messageQueue' $ Daemon.Start (LuaScript "test.lua")
        blockUntilNextEventLoop responseQueue
        atomically $ writeTQueue messageQueue' $ Daemon.Stop (LuaScript "test.lua")
        blockUntilNextEventLoop responseQueue
        lastLog <- atomically . readTQueue $ qLogger
        lastLog `shouldBe` "test.lua: cleanup"

threadMapSpecs :: Spec
threadMapSpecs = do
  around initAndCleanup $ do
    it "adds an entry to the ThreadMap List indexed by AutomationName" $
      testWithAsyncDaemon $ \_env daemonState messageQueue' responseQueue -> do
        atomically $ writeTQueue messageQueue' $ Daemon.Start Gold
        blockUntilNextEventLoop responseQueue
        threadMap' <- readTVarIO $ _threadMap daemonState
        lookupOrFail "Should have an action at index AutomationName `Gold`" Gold threadMap' $
          \testAutomations -> testAutomations ^? _1 . name `shouldBe` Just Gold

  around initAndCleanup $ do
    it "removes conflicting Automation entries using 'owned' Devices from ThreadMap when starting" $
      testWithAsyncDaemon $ \_env daemonState messageQueue' responseQueue -> do
        atomically $ writeTQueue messageQueue' $ Daemon.Start Gold
        blockUntilNextEventLoop responseQueue
        atomically $ writeTQueue messageQueue' $ Daemon.Start Gold
        blockUntilNextEventLoop responseQueue
        threadMap' <- readTVarIO $ _threadMap daemonState
        lookupOrFail "Should have an action at index AutomationName `Gold`" Gold threadMap' $
          \testAutomations -> length testAutomations `shouldBe` 1

  around initAndCleanup $ do
    it "removes entries from ThreadMap when stopping" $
      testWithAsyncDaemon $ \_env daemonState messageQueue' responseQueue -> do
        atomically $ writeTQueue messageQueue' $ Daemon.Start Gold
        blockUntilNextEventLoop responseQueue
        atomically $ writeTQueue messageQueue' $ Daemon.Stop Gold
        blockUntilNextEventLoop responseQueue
        threadMap' <- readTVarIO $ _threadMap daemonState
        -- the void hack here is because there is no Show
        -- instance for Just Automation, but there is one for Just (), and
        -- all I care about with this test is the effect, not the
        -- value
        (void . M.lookup Gold) threadMap' `shouldBe` Nothing

  around initAndCleanup $ do
    it "removes entries from ThreadMap for LuaScript automations as well after stopping" $
      testWithAsyncDaemon $ \_env daemonState messageQueue' responseQueue -> do
        atomically $ writeTQueue messageQueue' $ Daemon.Start (LuaScript "test.lua")
        blockUntilNextEventLoop responseQueue
        atomically $ writeTQueue messageQueue' $ Daemon.Stop (LuaScript "test.lua")
        blockUntilNextEventLoop responseQueue
        threadMap' <- readTVarIO $ _threadMap daemonState
        -- the void hack here is because there is no Show
        -- instance for Just Automation, but there is one for Just (), and
        -- all I care about with this test is the effect, not the
        -- value
        (void . M.lookup (LuaScript "test.lua")) threadMap' `shouldBe` Nothing

deviceMapSpecs :: Spec
deviceMapSpecs = do
  around initAndCleanup $ do
    it "adds an entry to the DeviceMap List indexed by DeviceId" $
      testWithAsyncDaemon $ \_env daemonState messageQueue' responseQueue -> do
        atomically $ writeTQueue messageQueue' $ Daemon.Start Gold
        blockUntilNextEventLoop responseQueue
        deviceMap' <- readTVarIO $ _deviceMap daemonState
        lookupOrFail
          "Should have an action at index AutomationName `Gold`" Gold.gledoptoLightStrip deviceMap'
            (\testAutomationNames -> length testAutomationNames `shouldBe` 1)

    it "removes AutomationName from the DeviceMap entry when the Automation using it is shut down" $
      testWithAsyncDaemon $ \_env daemonState messageQueue' responseQueue -> do
        atomically $ writeTQueue messageQueue' $ Daemon.Start Gold
        blockUntilNextEventLoop responseQueue
        atomically $ writeTQueue messageQueue' $ Daemon.Stop Gold
        blockUntilNextEventLoop responseQueue
        deviceMap' <- readTVarIO $ _deviceMap daemonState
        -- see comments above about void hack
        (void . M.lookup Gold.gledoptoLightStrip) deviceMap' `shouldBe` Nothing

    it "ensures proper bookkeeping for DeviceMap entries when an Automation is shut down due to another Automation starting" $
      testWithAsyncDaemon $ \_env daemonState messageQueue' responseQueue -> do
        atomically $ writeTQueue messageQueue' $ Daemon.Start Gold
        blockUntilNextEventLoop responseQueue
        atomically $ writeTQueue messageQueue' $ Daemon.Start Gold
        blockUntilNextEventLoop responseQueue
        deviceMap' <- readTVarIO $ _deviceMap daemonState
        lookupOrFail
          "Should have an action at index AutomationName `Gold`" Gold.gledoptoLightStrip deviceMap' $
            \testAutomationNames -> length testAutomationNames `shouldBe` 1


_schedulerSpecs :: Spec
_schedulerSpecs = do
  around initAndCleanup $ do
    it "schedules an action to be run at a later date" $
      testWithAsyncDaemon $ \_env daemonState messageQueue' responseQueue -> do
        atomically $
          writeTQueue messageQueue' $ Daemon.Schedule (Daemon.Start Gold) "* * * * *"
        -- we want to wait for two event loops--one for handling the
        -- Schedule message, and one for the Start message that will
        -- be sent later--so we call this twice:
        blockUntilNextEventLoop responseQueue
        blockUntilNextEventLoop responseQueue

        threadMap' <- readTVarIO $ _threadMap daemonState
        let goldAutomationEntry = M.lookup Gold threadMap'
        (void $ goldAutomationEntry) `shouldBe` (Just ())
