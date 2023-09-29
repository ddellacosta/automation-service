{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns -fno-warn-unused-imports #-}

module Test.Integration.Service.Daemon
  ( spec
  ,
  )
where

import Control.Exception (SomeException, handle)
import Control.Lens (_1, _2, _3, _Just, _head, folded, ix, preview, (<&>), (^.), (^?), (^..))
import Control.Monad (void)
import Data.Aeson (Value, decode, encode)
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens (_Array, _Object, key)
import qualified Data.ByteString.Char8 as SBS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (for_)
import qualified Data.HashMap.Strict as M
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List (null)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import GHC.Conc (ThreadStatus (..), threadStatus)
import Network.MQTT.Topic (mkTopic)
import qualified Network.WebSockets as WS
import Safe (headMay)
import Service.Automation (name)
import Service.AutomationName (AutomationName (..), parseAutomationName, serializeAutomationName)
import Service.Env (LoggerVariant (..), MQTTClientVariant (..), RestartConditions (..),
                    automationServiceTopic, config, daemonBroadcast, dbPath, deviceRegistrations,
                    devicesRawJSON, groupRegistrations, httpPort, logger, mqttClient, mqttConfig,
                    mqttDispatch, restartConditions, scheduledJobs)
import qualified Service.MQTT.Messages.Daemon as Daemon
import qualified Service.StateStore as StateStore
import System.Environment (setEnv)
import Test.Hspec (Spec, around, expectationFailure, it, shouldBe, xit)
import Test.Integration.Service.DaemonTestHelpers (initAndCleanup, testWithAsyncDaemon, waitUntilEq,
                                                   waitUntilEqSTM)
import UnliftIO.Async (asyncThreadId)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM (STM, atomically, readTChan, readTVar, readTVarIO, tryReadTChan, writeTChan,
                     writeTVar)

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
  schedulerSpecs
  statusMessageSpecs
  httpSpecs

--
-- The first two here seem to have a race condition because of
-- StateManager starting first at times. Probably just need to
-- refactor the test to consider the set of values in the first two
-- entries?
--
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

  -- flaky
  around initAndCleanup $ do
    it "returns Lua exception info when a Lua script run fails" $
      testWithAsyncDaemon $ \env _threadMapTV _daemonSnooper -> do
        let
          daemonBroadcast' = env ^. daemonBroadcast
          (QLogger qLogger) = env ^. logger
          expectedLogEntry = "Debug: LuaScript testBroken finished with status '\"Lua exception: attempt to call a string value\\nstack traceback:\"'."

        atomically $ writeTChan daemonBroadcast' $ Daemon.Start (LuaScript "testBroken")

        -- see comment in test below
        threadDelay 100000

        logs <- readTVarIO qLogger
        logEntryMatch <- pure . headMay . filter (== expectedLogEntry) $ logs

        logEntryMatch `shouldBe` Just expectedLogEntry

  around initAndCleanup $ do
    xit "allows scripts to register devices" $
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
    xit "allows scripts to register groups" $
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

  -- flaky
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
        -- Part 3: moved all the functions that weren't exposed at the
        -- module level to where terms, seemed to slow everything down
        -- somehow?
        --
        threadDelay 80000

        dispatchActions <- M.lookup topic <$> readTVarIO mqttDispatch'
        for_ (fromJust dispatchActions) ($ "{\"msg\": \"hey\"}")

        -- Probably the slowest part of the entire test suite. Would
        -- be good to find another way to test this. Also the lookup
        -- is kinda ugly.
        waitUntilEq expectedLogEntry $ do
          logs <- readTVarIO qLogger
          pure . fromMaybe "" . headMay . filter (== expectedLogEntry) $ logs

  -- flaky
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
        threadDelay 100000

        deviceRegs <- readTVarIO deviceRegistrations'
        M.lookup deviceId deviceRegs `shouldBe` (Just (LuaScript "testRegistration" :| []))

        groupRegs <- readTVarIO groupRegistrations'
        M.lookup groupId groupRegs `shouldBe` (Just (LuaScript "testRegistration" :| []))

        atomically $ writeTChan daemonBroadcast' $ Daemon.Stop (LuaScript "testRegistration")

        -- and here
        threadDelay 60000

        deviceRegs' <- readTVarIO deviceRegistrations'
        M.lookup deviceId deviceRegs' `shouldBe` Nothing

        groupRegs' <- readTVarIO groupRegistrations'
        M.lookup groupId groupRegs' `shouldBe` Nothing

  around initAndCleanup $ do
    xit "retrieves dates for Sun events (rise & set)" $
      testWithAsyncDaemon $ \env _threadMapTV _daemonSnooper -> do

        setEnv "TZ" "America/New_York"

        let
          daemonBroadcast' = env ^. daemonBroadcast
          (QLogger qLogger) = env ^. logger

        atomically $ writeTChan daemonBroadcast' $ Daemon.Start (LuaScript "testSunEvents")

        threadDelay 50000

        logs <- readTVarIO qLogger
        let
          matches = filter
            (\l ->
               (T.isPrefixOf "Debug: testSunEvents: sunrise: 202" l) ||
               (T.isPrefixOf "Debug: testSunEvents: sunset: 202" l)
            )
            logs

        length matches `shouldBe` 2

  around initAndCleanup $ do
    it "can send Daemon messages in Lua scripts" $
      testWithAsyncDaemon $ \env _threadMapTV daemonSnooper -> do
        let
          daemonBroadcast' = env ^. daemonBroadcast
          luaScriptSentMsg = Daemon.Start Gold

        atomically $ writeTChan daemonBroadcast' $
          Daemon.Start (LuaScript "testSendMsg")

        threadDelay 60000

        let
          getCurrentMsgBatch :: [Daemon.Message] -> STM [Daemon.Message]
          getCurrentMsgBatch msgs = tryReadTChan daemonSnooper >>=
            maybe (pure msgs) (\msg' -> getCurrentMsgBatch $ msg':msgs)

        msgs <- atomically $ getCurrentMsgBatch []

        (null $ filter (== luaScriptSentMsg) msgs) `shouldBe` False


threadMapSpecs :: Spec
threadMapSpecs = do
  around initAndCleanup $ do
    it "adds an entry to the ThreadMap List indexed by AutomationName" $
      testWithAsyncDaemon $ \env threadMapTV _daemonSnooper -> do
        let daemonBroadcast' = env ^. daemonBroadcast
        atomically $ writeTChan daemonBroadcast' $ Daemon.Start Gold

        waitUntilEqSTM (Just Gold) $
          preview (ix Gold . _1 . name) <$> readTVar threadMapTV

  around initAndCleanup $ do
    it "removes entries from ThreadMap when stopping" $
      testWithAsyncDaemon $ \env threadMapTV _daemonSnooper -> do
        let daemonBroadcast' = env ^. daemonBroadcast

        atomically $ writeTChan daemonBroadcast' $ Daemon.Start Gold
        atomically $ writeTChan daemonBroadcast' $ Daemon.Stop Gold

        threadDelay 100000

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
        threadDelay 60000

        threadMap <- readTVarIO threadMapTV
        -- same as above wrt void
        (void . M.lookup (LuaScript "test")) threadMap `shouldBe` Just ()

        atomically $ writeTChan daemonBroadcast' $ Daemon.Stop (LuaScript "test")
        threadDelay 20000

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
        threadDelay 60000

        threadMap <- readTVarIO threadMapTV
        case M.lookup Gold threadMap of
          Just (_, gold1Async) -> do
            let gold1ThreadId = asyncThreadId gold1Async
            atomically $ writeTChan daemonBroadcast' $ Daemon.Start Gold
            threadDelay 60000
            gold1ThreadStatus <- threadStatus gold1ThreadId
            gold1ThreadStatus `shouldBe` ThreadFinished
          Nothing -> expectationFailure "Couldn't find Gold instance 1 in threadMap"

  around initAndCleanup $ do
    it "removes entries from ThreadMap for automations when they are not shut down via message" $
      testWithAsyncDaemon $ \env threadMapTV _daemonSnooper -> do
        let daemonBroadcast' = env ^. daemonBroadcast

        atomically $ writeTChan daemonBroadcast' $ Daemon.Start (LuaScript "testNoLoop")
        threadDelay 100000

        threadMapNext <- readTVarIO threadMapTV
        (void . M.lookup (LuaScript "testNoLoop") $ threadMapNext)
          `shouldBe`
          Nothing


stateStoreSpecs :: Spec
stateStoreSpecs = do
  around initAndCleanup $ do
    it "stores the current set of running automations in sqlite" $
      testWithAsyncDaemon $ \env _threadMapTV _daemonSnooper -> do
        let
          daemonBroadcast' = env ^. daemonBroadcast

        -- let StateManager start up, or else this won't be recorded
        threadDelay 200000

        atomically $ writeTChan daemonBroadcast' $ Daemon.Start (LuaScript "test")

        threadDelay 200000

        res <- StateStore.allRunning $ env ^. config . dbPath

        -- HTTPDefault, HTTP <$config.httpPort>, StateManager, LuaScript "test"
        length res `shouldBe` 4

        parseAutomationName . T.unpack <$> (findMatchingSerialized "test" res)
          `shouldBe` [Just (LuaScript "test")]

        -- started up by Daemon independently if it is not running, so
        -- should always be present.
        findMatchingSerialized "StateManager" res `shouldBe` ["StateManager"]

  around initAndCleanup $ do
    it "updates stored automations when an automation is shut down" $
      testWithAsyncDaemon $ \env _threadMapTV _daemonSnooper -> do
        let
          daemonBroadcast' = env ^. daemonBroadcast
          dbPath' = env ^. config . dbPath

        atomically $ writeTChan daemonBroadcast' $ Daemon.Start Gold
        atomically $ writeTChan daemonBroadcast' $ Daemon.Start (LuaScript "test")

        threadDelay 80000

        atomically $ writeTChan daemonBroadcast' $ Daemon.Stop Gold

        threadDelay 80000

        res <- StateStore.allRunning dbPath'

        -- HTTPDefault, HTTP <$config.httpPort>, StateManager, LuaScript "test"
        length res `shouldBe` 4

        parseAutomationName . T.unpack <$> (findMatchingSerialized "test" res)
          `shouldBe` [Just (LuaScript "test")]
        -- started up by Daemon independently if it is not running, so
        -- should always be present.
        findMatchingSerialized "StateManager" res `shouldBe` ["StateManager"]

  around initAndCleanup $ do
    it "starts any automations stored in the running table upon load" $ \preEnv -> do
      StateStore.updateRunning (preEnv ^. config . dbPath) $
        --
        -- For some reason, having more than one blocks before the
        -- Null msg sending below. Works fine in production with
        -- multiple automations being restarted though? Hmm
        --
        -- serializeAutomationName <$> [LuaScript "test",  LuaScript "testAgain"]
        serializeAutomationName <$> [LuaScript "test"]
      flip testWithAsyncDaemon preEnv $ \env threadMapTV _daemonSnooper -> do
        let
          daemonBroadcast' = env ^. daemonBroadcast
          restartConditions' = env ^. restartConditions

        --
        -- I added this because the bug I discovered in production
        -- (*cough* a.k.a. my house) isn't exposed unless I give
        -- things a bit more of a delay at startup, to mimic the
        -- behavior I'm seeing there. Once I added the delay, this
        -- test started failing. This failure was caused by calling
        -- Service.StateStore.updateRunning every time we start up an
        -- Automation, because the first thing it does is wipe out the
        -- prior running entries before storing the new set of
        -- Automations that are passed in. This keeps things nice and
        -- simple semantically once the system is up and running, as
        -- it lacks any updating logic that would produce a bug based
        -- on doing e.g. set difference calculations or whatnot, but
        -- it means we push the logic for updates outside of the DB
        -- layer. Putting it all together, concretely speaking this
        -- means that as soon as we started the first
        -- Automation--which is StateManager by default, as it is
        -- explicitly loaded in Service.Daemon--the previously running
        -- Automation state stored in the db was wiped out.
        --
        -- The solution I came up with was to cache the stored
        -- DB values on initialization and reference this cache once
        -- the conditions encoded in RestartConditions are met, and
        -- tryRestoreRunningAutomations loads the prior Automations.
        --
        threadDelay 50000

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

        -- (Minor side-note, previously was implementing this like
        -- this, updated in case that was responsible for the blocking
        -- with >1 Automations, but no dice...leaving this comment and
        -- commented code below here as a clue to future me or whoever
        -- in case it is useful.)
        --
        -- atomically $ writeTChan daemonBroadcast' $ Daemon.DeviceUpdate []
        -- atomically $ writeTChan daemonBroadcast' $ Daemon.GroupUpdate []
        --
        atomically . writeTVar restartConditions' $ RestartConditions True True True
        threadDelay 50000

        atomically . writeTChan daemonBroadcast' $ Daemon.Null
        threadDelay 50000

        runningAutos <- M.keys <$> readTVarIO threadMapTV

        filter (== LuaScript "test") runningAutos
          `shouldBe` [LuaScript "test"]

        -- see above re: locking up with more than one automation
        -- filter (== LuaScript "testAgain") runningAutos
        --   `shouldBe` [LuaScript "testAgain"]

        filter (== StateManager) runningAutos `shouldBe` [StateManager]

  around initAndCleanup $ do
    it "stores the current set of scheduled automations in sqlite" $
      testWithAsyncDaemon $ \env _threadMapTV _daemonSnooper -> do
        let
          daemonBroadcast' = env ^. daemonBroadcast
          scheduleMsg =
            Daemon.Schedule "test" "0 6 * * *" $ Daemon.Start (LuaScript "test")

        -- let StateManager start up, or else this won't be recorded
        threadDelay 200000

        atomically $ writeTChan daemonBroadcast' scheduleMsg

        -- without this seems to lock up DB, presumably in conflict with
        -- write happening in Daemon as a result of scheduling? I
        -- think I need to put the dbPath in a TVar or something to
        -- enforce atomic updates to the DB
        threadDelay 200000

        res <- StateStore.allScheduled $ env ^. config . dbPath

        res ^? _head . _2 `shouldBe`
          (Just . encodeStrict $ scheduleMsg)

  around initAndCleanup $ do
    it "starts previously scheduled automations when starting" $ \preEnv -> do
      let
        scheduleMsg =
          Daemon.Schedule "test" "0 6 * * *" $ Daemon.Start (LuaScript "test")

      StateStore.updateScheduled (preEnv ^. config . dbPath) $ [encodeStrict scheduleMsg]

      flip testWithAsyncDaemon preEnv $ \env _threadMapTV _daemonSnooper -> do
        let
          daemonBroadcast' = env ^. daemonBroadcast
          restartConditions' = env ^. restartConditions

        atomically . writeTVar restartConditions' $ RestartConditions True True True
        threadDelay 200000

        atomically $ writeTChan daemonBroadcast' (Daemon.Unschedule "test")
        threadDelay 200000

        res <- StateStore.allScheduled $ env ^. config . dbPath

        length res `shouldBe` 0

  around initAndCleanup $ do
    it "starts previously scheduled automations when starting" $ \preEnv -> do
      let
        scheduleMsg =
          Daemon.Schedule "test" "0 6 * * *" $ Daemon.Start (LuaScript "test")

      StateStore.updateScheduled (preEnv ^. config . dbPath) $ [encodeStrict scheduleMsg]

      flip testWithAsyncDaemon preEnv $ \env _threadMapTV _daemonSnooper -> do
        let
          restartConditions' = env ^. restartConditions

        atomically . writeTVar restartConditions' $ RestartConditions True True True
        threadDelay 200000

        scheduled <- readTVarIO $ env ^. scheduledJobs
        let testJob = M.lookup "test" scheduled

        testJob ^? _Just . _1 `shouldBe` Just "0 6 * * *"
        testJob ^? _Just . _2 `shouldBe` Just (Daemon.Start (LuaScript "test"))

  where
    findMatchingSerialized :: (Eq a) => a -> [(Int, a)] -> [a]
    findMatchingSerialized serialized res =
      snd <$> filter (\(_id, serialized') -> serialized' == serialized) res

    encodeStrict = toStrictBS . encode

schedulerSpecs :: Spec
schedulerSpecs = do
  around initAndCleanup $ do
    it "stores Schedule jobs when they come in, and removes then when Unscheduled" $
      testWithAsyncDaemon $ \env _threadMapTV _daemonSnooper -> do
        let
          daemonBroadcast' = env ^. daemonBroadcast
          scheduledJobs' = env ^. scheduledJobs
          jobId = "gold1"
          jobSchedule = "* * * * *"
          goldStartMsg = Daemon.Start Gold

        atomically $ writeTChan daemonBroadcast' $
          Daemon.Schedule jobId jobSchedule goldStartMsg

        threadDelay 50000

        jobs <- readTVarIO scheduledJobs'
        jobs ^? ix jobId . _1 `shouldBe` Just jobSchedule
        jobs ^? ix jobId . _2 `shouldBe` Just goldStartMsg

        atomically $ writeTChan daemonBroadcast' $
          Daemon.Unschedule jobId

        threadDelay 50000

        jobs' <- readTVarIO scheduledJobs'
        M.lookup jobId jobs' `shouldBe` Nothing

        let Just gold1ThreadId = jobs ^? ix jobId . _3
        gold1ThreadStatus <- threadStatus gold1ThreadId
        gold1ThreadStatus `shouldBe` ThreadFinished

statusMessageSpecs :: Spec
statusMessageSpecs = do
  around initAndCleanup $ do
    it "sends a status message when Daemon.Status is received" $
      testWithAsyncDaemon $ \env _threadMapTV _daemonSnooper -> do
        let
          daemonBroadcast' = env ^. daemonBroadcast
          automationServiceTopic' = env ^. config . mqttConfig . automationServiceTopic
          (TVClient topicMapTV) = env ^. mqttClient

        atomically $ writeTChan daemonBroadcast' Daemon.Status

        threadDelay 50000

        topicMap <- readTVarIO topicMapTV

        let
          autoServiceTopic = do
            decoded :: Maybe Value <- decode =<< M.lookup automationServiceTopic' topicMap
            Just $ decoded ^.. _Just . key "runningAutomations" . _Array . folded . key "name"

        (null . filter (== Aeson.String "HTTPDefault")) <$> autoServiceTopic
          `shouldBe`
          Just False

        (null . filter (== Aeson.String "StateManager")) <$> autoServiceTopic
          `shouldBe`
          Just False


httpSpecs :: Spec
httpSpecs = do
  around initAndCleanup $ do
    it "sends device data over websockets" $
      testWithAsyncDaemon $ \env _threadMapTV _daemonSnooper -> do
        let
          port = env ^. config . httpPort
          devices = env ^. devicesRawJSON

        -- HTTPDefault is run by default on start

        -- not sure why "localhost" doesn't work vs. "127.0.0.1",
        -- probably something basic I'm forgetting
        devicesReceived <- retry $ WS.runClient "127.0.0.1" (fromIntegral port) "" $
          \conn -> do
            msg <- WS.receiveData conn
            WS.sendClose conn ("close" :: Text)
            pure msg

        devices' <- readTVarIO devices
        devicesReceived `shouldBe` devices'

    it "allows a websockets client to publish an MQTT message" $
      testWithAsyncDaemon $ \env _threadMapTV daemonSnooper -> do
        let
          daemonBroadcast' = env ^. daemonBroadcast
          (TVClient mqttMsgs) = env ^. mqttClient
          topicStr = "/device/lamp/set"
          Just topic = mkTopic . T.decodeUtf8Lenient . toStrictBS $ topicStr

          -- +1 to make sure we don't try to use the same port when
          -- these tests run in parallel.
          port = 1 + (env ^. config . httpPort)

        atomically $ writeTChan daemonBroadcast' (Daemon.Start (HTTP port))

        retry $ WS.runClient "127.0.0.1" (fromIntegral port) "" $
          \conn -> do
            WS.sendTextData conn ("{\"start\": \"test\"}" :: ByteString)
            WS.sendTextData conn
              (  "{\"publish\": {\"state\": \"ON\"}, "
              <>  "\"topic\": \"" <> topicStr <> "\""
              <>  "}"
              :: ByteString
              )
            WS.sendClose conn ("close" :: Text)

        -- seem to need this to let the messages accumulate
        threadDelay 50000

        let
          getCurrentMsgBatch :: [Daemon.Message] -> STM [Daemon.Message]
          getCurrentMsgBatch msgs = tryReadTChan daemonSnooper >>=
            maybe (pure msgs) (\msg' -> getCurrentMsgBatch $ msg':msgs)

        msgs <- atomically $ getCurrentMsgBatch []
        (null $ filter (== Daemon.Start (LuaScript "test")) msgs) `shouldBe` False

        receivedMsg <- M.lookup topic <$> readTVarIO mqttMsgs
        receivedMsg `shouldBe` Just "{\"state\":\"ON\"}"

  where
    --
    -- stolen from
    --   https://github.com/jaspervdj/websockets/blob/72b6a7223220c90e0045930e4ef7e771f010be92/tests/haskell/Network/WebSockets/Server/Tests.hs#L119-L129
    --
    retry :: IO a -> IO a
    retry action = handle
      (\(_ :: SomeException) -> (threadDelay 2000000) >> retry action)
      action

toStrictBS :: LBS.ByteString -> SBS.ByteString
toStrictBS = SBS.concat . LBS.toChunks
