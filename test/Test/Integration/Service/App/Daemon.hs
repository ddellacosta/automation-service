{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Integration.Service.App.Daemon
  ( spec
  ,
  )
where

import Control.Lens ((^.), (^?), _1, ix, preview)
import Control.Monad (void)
import Data.Foldable (for_, forM_)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Network.MQTT.Topic (mkTopic)
import Safe (headMay)
import Service.Automation (name)
import Service.AutomationName (AutomationName(..))
import Service.Env
  ( LoggerVariant(..)
  , daemonBroadcast
  , deviceRegistrations
  , logger
  , mqttDispatch
  )
import qualified Service.Messages.Daemon as Daemon
import Test.Hspec (Spec, around, it, shouldBe)
import Test.Integration.Service.App.DaemonTestHelpers
  ( initAndCleanup
  , testWithAsyncDaemon
  , waitUntilEq
  )
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM (atomically, readTChan, readTVar, writeTChan)

spec :: Spec
spec = do
  deviceRegistrationSpecs
  -- this was timing out a bunch but now seems fine...?
  luaScriptSpecs
  threadMapSpecs

  -- TODO: Haven't yet figured out how to test scheduler
  -- functionality. Would like to be able to do something similar to
  -- Ruby's timecop but haven't worked out the details there yet.
  -- Used to have some super slow tests that showed the scheduler is
  -- correct at least on a basic level, but due to some refactoring
  -- the way it worked didn't make sense any more, and rather than
  -- refactor a pointlessly slow test, I just scrapped it.

deviceRegistrationSpecs :: Spec
deviceRegistrationSpecs = do
  around initAndCleanup $ do
    it "allows for device registration" $
      testWithAsyncDaemon $ \env _threadMapTV daemonSnooper -> do
        let
          mirrorLightID = "0xb4e3f9fffe14c707"
          daemonBroadcast' = env ^. daemonBroadcast
          deviceRegs = env ^. deviceRegistrations
          regGoldMsgIn = Daemon.Register mirrorLightID Gold

        atomically $ writeTChan daemonBroadcast' regGoldMsgIn

        regGoldMsgOut <- atomically $ readTChan daemonSnooper
        regGoldMsgOut `shouldBe` regGoldMsgIn

        waitUntilEq (Just Gold) $
          (readTVar deviceRegs >>= pure . M.lookup mirrorLightID)

  around initAndCleanup $ do
    it "sends a Stop message to the daemonBroadcast TChan for running automation when device is registered" $
      testWithAsyncDaemon $ \env _threadMapTV daemonSnooper -> do
        let
          mirrorLightID = "0xb4e3f9fffe14c707"
          daemonBroadcast' = env ^. daemonBroadcast
          deviceRegs = env ^. deviceRegistrations

        atomically $ writeTChan daemonBroadcast' $ Daemon.Register mirrorLightID Gold
        atomically $ writeTChan daemonBroadcast' $ Daemon.Register mirrorLightID (LuaScript "test.lua")

        regStopGoldMsg <- atomically $ do
          void $ readTChan daemonSnooper -- register Gold
          void $ readTChan daemonSnooper -- register LuaScript
          readTChan daemonSnooper

        regStopGoldMsg `shouldBe` Daemon.Stop Gold

        waitUntilEq (Just (LuaScript "test.lua")) $
          readTVar deviceRegs >>= pure . M.lookup mirrorLightID

luaScriptSpecs :: Spec
luaScriptSpecs = do
  around initAndCleanup $ do
    it "allows scripts to register devices" $
      testWithAsyncDaemon $ \env _threadMapTV _daemonSnooper -> do
        let
          daemonBroadcast' = env ^. daemonBroadcast
          mirrorLightID = "0xb4e3f9fffe14c707"
          registrations = env ^. deviceRegistrations

        atomically $ writeTChan daemonBroadcast' $ Daemon.Start Gold
        atomically $ writeTChan daemonBroadcast' $ Daemon.Start (LuaScript "testDSL.lua")

        waitUntilEq (Just (LuaScript "testDSL.lua")) $
          readTVar registrations >>= pure . M.lookup mirrorLightID

  around initAndCleanup $ do
    it "starts and shuts down a Lua script" $
      testWithAsyncDaemon $ \env threadMapTV _daemonSnooper -> do
        let
          daemonBroadcast' = env ^. daemonBroadcast

        atomically $ writeTChan daemonBroadcast' $ Daemon.Start (LuaScript "test.lua")

        waitUntilEq (Just (LuaScript "test.lua")) $
          readTVar threadMapTV >>= pure . preview (ix (LuaScript "test.lua") . _1 . name)

        atomically $ writeTChan daemonBroadcast' $ Daemon.Stop (LuaScript "test.lua")

        waitUntilEq Nothing $
          readTVar threadMapTV >>= pure . preview (ix (LuaScript "test.lua") . _1 . name)

  around initAndCleanup $ do
    it "subscribes to topic and receives topic messages" $
      testWithAsyncDaemon $ \env _threadMapTV _daemonSnooper -> do
        let
          daemonBroadcast' = env ^. daemonBroadcast
          (QLogger qLogger) = env ^. logger
          mqttDispatch' = env ^. mqttDispatch
          Just topic = mkTopic "a/b/c"
          expectedLogEntry = "Debug: testSubscribe.lua: Msg: hey"

        atomically $ writeTChan daemonBroadcast' $ Daemon.Start (LuaScript "testSubscribe.lua")

        -- seems like without a small wait here, the read on
        -- mqttDispatch' below produces a deadlock on that TVar and
        -- makes this time out
        threadDelay 10000

        dispatchStore <- atomically $ readTVar mqttDispatch'
        let maybeActions = dispatchStore ^? ix topic
        for_ maybeActions $ \actions ->
          forM_ actions ($ "{\"msg\": \"hey\"}")

        let logEntryFind = do
              logs <- readTVar qLogger
              pure . fromMaybe "" . headMay . filter (== expectedLogEntry) $ logs

        -- Probably the slowest part of the entire test suite. Would
        -- be good to find another way to test this.
        waitUntilEq expectedLogEntry logEntryFind

threadMapSpecs :: Spec
threadMapSpecs = do
  around initAndCleanup $ do
    it "adds an entry to the ThreadMap List indexed by AutomationName" $
      testWithAsyncDaemon $ \env threadMapTV _daemonSnooper -> do
        let daemonBroadcast' = env ^. daemonBroadcast
        atomically $ writeTChan daemonBroadcast' $ Daemon.Start Gold

        waitUntilEq (Just Gold) $
          readTVar threadMapTV >>= pure . preview (ix Gold . _1 . name)

  around initAndCleanup $ do
    it "removes entries from ThreadMap when stopping" $
      testWithAsyncDaemon $ \env threadMapTV _daemonSnooper -> do
        let daemonBroadcast' = env ^. daemonBroadcast
        atomically $ writeTChan daemonBroadcast' $ Daemon.Start Gold
        atomically $ writeTChan daemonBroadcast' $ Daemon.Stop Gold
        threadMap <- atomically . readTVar $ threadMapTV
        -- the void hack here is because there is no Show
        -- instance for Just Automation, but there is one for Just (), and
        -- all I care about with this test is the effect, not the
        -- value
        (void . M.lookup Gold) threadMap `shouldBe` Nothing

  around initAndCleanup $ do
    it "removes entries from ThreadMap for LuaScript automations as well after stopping" $
      testWithAsyncDaemon $ \env threadMapTV _daemonSnooper -> do
        let daemonBroadcast' = env ^. daemonBroadcast
        atomically $ writeTChan daemonBroadcast' $ Daemon.Start (LuaScript "test.lua")
        atomically $ writeTChan daemonBroadcast' $ Daemon.Stop (LuaScript "test.lua")
        threadMap <- atomically . readTVar $ threadMapTV
        -- the void hack here is because there is no Show
        -- instance for Just Automation, but there is one for Just (), and
        -- all I care about with this test is the effect, not the
        -- value
        (void . M.lookup (LuaScript "test.lua")) threadMap `shouldBe` Nothing
