{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Integration.Service.Daemon
  ( spec
  ,
  )
where

import Control.Lens ((^.), (<&>), _1, ix, preview)
import Control.Monad (void)
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.HashMap.Strict as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import GHC.Conc (ThreadStatus(..), threadStatus)
import Network.MQTT.Topic (mkTopic)
import Safe (headMay)
import Service.Automation (name)
import Service.AutomationName (AutomationName(..), serializeAutomationName)
import Service.Env
  ( LoggerVariant(..)
  , config
  , daemonBroadcast
  , dbPath
  , deviceRegistrations
  , groupRegistrations
  , logger
  , mqttDispatch
  )
import qualified Service.MQTT.Messages.Daemon as Daemon
import qualified Service.StateStore as StateStore
import Test.Hspec (Spec, around, expectationFailure, it, shouldBe)
import Test.Integration.Service.DaemonTestHelpers
  ( initAndCleanup
  , testWithAsyncDaemon
  , waitUntilEq
  , waitUntilEqSTM
  )
import UnliftIO.Async (asyncThreadId)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM (atomically, readTChan, readTVar, readTVarIO, writeTChan)

-- TODO: Haven't yet figured out how to test scheduler
-- functionality. Would like to be able to do something similar to
-- Ruby's timecop but haven't worked out the details there yet.
-- Used to have some super slow tests that showed the scheduler is
-- correct at least on a basic level, but due to some refactoring
-- the way it worked didn't make sense any more, and rather than
-- refactor a pointlessly slow test, I just scrapped it.

spec :: Spec
spec = do
  -- this was timing out a bunch but now seems fine...?
  luaScriptSpecs
  resourceRegistrationSpecs
  threadMapSpecs
  stateStoreSpecs

resourceRegistrationSpecs :: Spec
resourceRegistrationSpecs = do
  around initAndCleanup $ do
    it "allows for device registration" $
      testWithAsyncDaemon $ \env _threadMapTV daemonSnooper -> do
        let
          mirrorLightID = "0xb4e3f9fffe14c707"
          daemonBroadcast' = env ^. daemonBroadcast
          deviceRegs = env ^. deviceRegistrations
          regGoldMsgIn = Daemon.RegisterDevice mirrorLightID Gold

        atomically $ writeTChan daemonBroadcast' regGoldMsgIn

        regGoldMsgOut <- atomically $ readTChan daemonSnooper
        regGoldMsgOut `shouldBe` regGoldMsgIn

        waitUntilEqSTM (Just (Gold :| [])) $
          readTVar deviceRegs <&> M.lookup mirrorLightID

  around initAndCleanup $ do
    it "allows for group registration" $
      testWithAsyncDaemon $ \env _threadMapTV daemonSnooper -> do
        let
          basementStandingLampGroupId = 1
          daemonBroadcast' = env ^. daemonBroadcast
          groupRegs = env ^. groupRegistrations
          regGoldMsgIn = Daemon.RegisterGroup basementStandingLampGroupId Gold

        atomically $ writeTChan daemonBroadcast' regGoldMsgIn

        regGoldMsgOut <- atomically $ readTChan daemonSnooper
        regGoldMsgOut `shouldBe` regGoldMsgIn

        waitUntilEqSTM (Just (Gold :| [])) $
          readTVar groupRegs <&> M.lookup basementStandingLampGroupId


luaScriptSpecs :: Spec
luaScriptSpecs = do
  around initAndCleanup $ do
    it "starts and shuts down a Lua script" $
      testWithAsyncDaemon $ \env threadMapTV _daemonSnooper -> do
        let
          daemonBroadcast' = env ^. daemonBroadcast

        atomically $ writeTChan daemonBroadcast' $ Daemon.Start (LuaScript "test")

        waitUntilEqSTM (Just (LuaScript "test")) $
          readTVar threadMapTV <&> preview (ix (LuaScript "test") . _1 . name)

        atomically $ writeTChan daemonBroadcast' $ Daemon.Stop (LuaScript "test")

        waitUntilEq Nothing $ atomically $
          readTVar threadMapTV <&> preview (ix (LuaScript "test") . _1 . name)

  around initAndCleanup $ do
    it "returns Lua exception info when a Lua script run fails" $
      testWithAsyncDaemon $ \env _threadMapTV _daemonSnooper -> do
        let
          daemonBroadcast' = env ^. daemonBroadcast
          (QLogger qLogger) = env ^. logger
          expectedLogEntry = "Debug: LuaScript testBroken finished with status '\"Lua exception: attempt to call a string value\\nstack traceback:\"'."

        atomically $ writeTChan daemonBroadcast' $ Daemon.Start (LuaScript "testBroken")

        -- see comment in test below
        threadDelay 50000

        logs <- readTVarIO qLogger
        logEntryMatch <- pure . headMay . filter (== expectedLogEntry) $ logs

        logEntryMatch `shouldBe` Just expectedLogEntry

  around initAndCleanup $ do
    it "allows scripts to register devices" $
      testWithAsyncDaemon $ \env _threadMapTV _daemonSnooper -> do
        let
          daemonBroadcast' = env ^. daemonBroadcast
          mirrorLightID = "0xb4e3f9fffe14c707"
          registrations = env ^. deviceRegistrations

        atomically $ writeTChan daemonBroadcast' $ Daemon.Start Gold
        -- artificially enforce ordering of messages to conform to assertion below
        threadDelay 100000
        atomically $ writeTChan daemonBroadcast' $ Daemon.Start (LuaScript "testRegistration")

        -- see comment in test below
        threadDelay 10000

        mirrorLightAutos <- readTVarIO registrations <&> M.lookup mirrorLightID
        mirrorLightAutos
          `shouldBe`
          (Just (LuaScript "testRegistration" :| [Gold]))

  around initAndCleanup $ do
    it "allows scripts to register groups" $
      testWithAsyncDaemon $ \env _threadMapTV _daemonSnooper -> do
        let
          daemonBroadcast' = env ^. daemonBroadcast
          basementStandingLampGroupId = 1
          registrations = env ^. groupRegistrations

        atomically $ writeTChan daemonBroadcast' $ Daemon.Start Gold
        -- artificially enforce ordering of messages to conform to assertion below
        threadDelay 100000
        atomically $ writeTChan daemonBroadcast' $ Daemon.Start (LuaScript "testRegistration")

        -- see comment in test below
        threadDelay 10000

        basementStandingLampGroupAutos <- readTVarIO registrations <&> M.lookup basementStandingLampGroupId
        basementStandingLampGroupAutos
          `shouldBe`
          (Just (LuaScript "testRegistration" :| [Gold]))

  -- I don't love this test
  around initAndCleanup $ do
    it "subscribes to topic and receives topic messages" $
      testWithAsyncDaemon $ \env _threadMapTV _daemonSnooper -> do
        let
          daemonBroadcast' = env ^. daemonBroadcast
          (QLogger qLogger) = env ^. logger
          mqttDispatch' = env ^. mqttDispatch
          Just topic = mkTopic "testTopic"
          expectedLogEntry = "Debug: testSubscribe: Msg: hey"

        atomically $ writeTChan daemonBroadcast' $ Daemon.Start (LuaScript "testSubscribe")

        --
        -- Seems like without a small wait here, the read on
        -- mqttDispatch' below produces a deadlock on that TVar and
        -- makes this time out, which I guess I should have assumed?
        --
        -- Somehow I thought readTVarIO wouldn't behave that way
        -- because of docs [0] saying that, "this is equivalent to
        -- `atomically . readTVar` but works much faster, because it
        -- doesn't perform a complete transaction," but I guess I'm
        -- misunderstanding? Or, is what is causing the deadlock/timeout
        -- not the read on the mqttDispatch TVar?
        --
        -- Previously I was doing `readTVarIO mqttDispatch'` in a
        -- loop, and then tried it with the retry package (retrying
        -- with various policies, including backoff and explicit
        -- delays for a fixed number of times). Finally I realized
        -- that, no matter what, if I didn't have the delay here it
        -- would lock up, and otherwise I didn't really need to do
        -- anything but check it once like I'm now doing below. but I
        -- still don't understand why it doesn't work without this
        -- delay first.
        --
        -- NOTE this was written before I switched from using
        -- tryReadTChan (does not block, returns Nothing when nothing
        -- is present, so it is appropriate for polling) to readTChan
        -- (blocks) in LuaScript subscribe functions. the readTVarIO
        -- lookup does not block any more, but I still need the
        -- threadDelay.
        --
        -- [0] https://hackage.haskell.org/package/base-4.16.3.0/docs/GHC-Conc.html#v:readTVarIO
        --
        -- SECOND NOTE: update after added sqlite state-storage to the
        -- mix, now the smallest I can make this without having tests
        -- fail is 60000 microseconds.
        --
        threadDelay 60000

        dispatchActions <- M.lookup topic <$> readTVarIO mqttDispatch'
        for_ (fromJust dispatchActions) ($ "{\"msg\": \"hey\"}")

        -- Probably the slowest part of the entire test suite. Would
        -- be good to find another way to test this. Also the lookup
        -- is kinda ugly.
        waitUntilEq expectedLogEntry $ do
          logs <- readTVarIO qLogger
          pure . fromMaybe "" . headMay . filter (== expectedLogEntry) $ logs

  around initAndCleanup $ do
    it "removes Device and Group Registration entries upon cleanup" $
      testWithAsyncDaemon $ \env _threadMapTV _daemonSnooper -> do
        let
          daemonBroadcast' = env ^. daemonBroadcast
          deviceRegistrations' = env ^. deviceRegistrations
          deviceId = "0xb4e3f9fffe14c707"
          groupRegistrations' = env ^. groupRegistrations
          groupId = 1

        atomically $ writeTChan daemonBroadcast' $ Daemon.Start (LuaScript "testRegistration")

        -- same as above...don't love it here either
        threadDelay 60000

        deviceRegs <- readTVarIO deviceRegistrations'
        M.lookup deviceId deviceRegs `shouldBe` (Just (LuaScript "testRegistration" :| []))

        groupRegs <- readTVarIO groupRegistrations'
        M.lookup groupId groupRegs `shouldBe` (Just (LuaScript "testRegistration" :| []))

        atomically $ writeTChan daemonBroadcast' $ Daemon.Stop (LuaScript "testRegistration")

        -- and here
        threadDelay 10000

        deviceRegs' <- readTVarIO deviceRegistrations'
        M.lookup deviceId deviceRegs' `shouldBe` Nothing

        groupRegs' <- readTVarIO groupRegistrations'
        M.lookup groupId groupRegs' `shouldBe` Nothing


threadMapSpecs :: Spec
threadMapSpecs = do
  around initAndCleanup $ do
    it "adds an entry to the ThreadMap List indexed by AutomationName" $
      testWithAsyncDaemon $ \env threadMapTV _daemonSnooper -> do
        let daemonBroadcast' = env ^. daemonBroadcast
        atomically $ writeTChan daemonBroadcast' $ Daemon.Start Gold

        waitUntilEqSTM (Just Gold) $
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

        atomically $ writeTChan daemonBroadcast' $ Daemon.Start (LuaScript "test")
        threadDelay 50000

        threadMap <- readTVarIO threadMapTV
        -- same as above wrt void
        (void . M.lookup (LuaScript "test")) threadMap `shouldBe` Just ()

        atomically $ writeTChan daemonBroadcast' $ Daemon.Stop (LuaScript "test")
        threadDelay 10000

        threadMap' <- readTVarIO threadMapTV
        -- same as above wrt void
        (void . M.lookup (LuaScript "test")) threadMap' `shouldBe` Nothing

  around initAndCleanup $ do
    --
    -- This preserves this semantics that starting an Automation that
    -- is already running is equivalent to restarting restarting the
    -- Automation.
    --
    it "shuts previously running Automation when a duplicate is started" $
      testWithAsyncDaemon $ \env threadMapTV _daemonSnooper -> do
        let daemonBroadcast' = env ^. daemonBroadcast

        atomically $ writeTChan daemonBroadcast' $ Daemon.Start Gold
        threadDelay 50000

        threadMap <- readTVarIO threadMapTV
        case M.lookup Gold threadMap of
          Just (_, gold1Async) -> do
            let gold1ThreadId = asyncThreadId gold1Async
            atomically $ writeTChan daemonBroadcast' $ Daemon.Start Gold
            threadDelay 50000
            gold1ThreadStatus <- threadStatus gold1ThreadId
            gold1ThreadStatus `shouldBe` ThreadFinished
          Nothing -> expectationFailure "Couldn't find Gold instance 1 in threadMap"

stateStoreSpecs :: Spec
stateStoreSpecs = do
  around initAndCleanup $ do
    it "stores the current set of running automations in sqlite" $
      testWithAsyncDaemon $ \env _threadMapTV _daemonSnooper -> do
        let
          daemonBroadcast' = env ^. daemonBroadcast

        atomically $ writeTChan daemonBroadcast' $ Daemon.Start Gold
        atomically $ writeTChan daemonBroadcast' $ Daemon.Start (LuaScript "test")

        threadDelay 200000

        res <- StateStore.allRunning $ env ^. config . dbPath

        length res `shouldBe` 3
        findMatchingAutoNames "LuaScript \"test\"" res
          `shouldBe` ["LuaScript \"test\""]
        findMatchingAutoNames "Gold" res `shouldBe` ["Gold"]
        -- started up by Daemon independently if it is not running, so
        -- should always be present.
        findMatchingAutoNames "StateManager" res `shouldBe` ["StateManager"]

  around initAndCleanup $ do
    it "updates stored automations when an automation is shut down" $
      testWithAsyncDaemon $ \env _threadMapTV _daemonSnooper -> do
        let
          daemonBroadcast' = env ^. daemonBroadcast
          dbPath' = env ^. config . dbPath

        atomically $ writeTChan daemonBroadcast' $ Daemon.Start Gold
        atomically $ writeTChan daemonBroadcast' $ Daemon.Start (LuaScript "test")

        threadDelay 50000

        atomically $ writeTChan daemonBroadcast' $ Daemon.Stop Gold

        threadDelay 50000

        res <- StateStore.allRunning dbPath'

        length res `shouldBe` 2
        findMatchingAutoNames "LuaScript \"test\"" res
          `shouldBe` ["LuaScript \"test\""]
        -- started up by Daemon independently if it is not running, so
        -- should always be present.
        findMatchingAutoNames "StateManager" res `shouldBe` ["StateManager"]

  around initAndCleanup $ do
    it "starts any automations stored in the running table upon load" $ \preEnv -> do
      let autoNames = serializeAutomationName <$> [Gold, LuaScript "test"]
      StateStore.updateRunning (preEnv ^. config . dbPath) autoNames
      flip testWithAsyncDaemon preEnv $ \env threadMapTV _daemonSnooper -> do
        let daemonBroadcast' = env ^. daemonBroadcast

        --
        -- This is because scripts are often dependent on loading
        -- groups and devices--and I don't device/group registration
        -- to block in scripts, even if I have some kind of
        -- exponential backoff failure thing which eventually logs,
        -- since regardless that will prevent speedy diagnosis when
        -- trying to iterate on an automation.
        --
        -- As a result, I added the RestartConditions data structure
        -- to Env which needs all of its members to evaluate to True
        -- before a restart is tried in Daemon's main go loop (at
        -- which point the value that is set to True on
        -- initialization--notAlreadyRestarted--is set to False, and
        -- never updated again).
        --
        -- At some point I think it would make sense to toggle a
        -- setting to just automatically restart without all the
        -- conditions being met (maybe this makes sense if using all
        -- ESPHome devices? Not sure yet), but this is the default
        -- while I'm so heavily dependent on Zigbee2MQTT.
        --
        atomically $ writeTChan daemonBroadcast' $ Daemon.DeviceUpdate []
        atomically $ writeTChan daemonBroadcast' $ Daemon.GroupUpdate []

        threadDelay 50000

        runningAutos <- M.keys <$> readTVarIO threadMapTV

        filter (== Gold) runningAutos `shouldBe` [Gold]
        filter (== LuaScript "test") runningAutos
          `shouldBe` [LuaScript "test"]
        filter (== StateManager) runningAutos `shouldBe` [StateManager]

  where
    findMatchingAutoNames :: Text -> [(Int, Text)] -> [Text]
    findMatchingAutoNames autoName res =
      (snd <$> (filter (\(_id, auto) -> auto == autoName) res))