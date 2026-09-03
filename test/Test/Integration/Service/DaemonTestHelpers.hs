module Test.Integration.Service.DaemonTestHelpers
  ( TestLogger(..)
  , TestMQTTClient(..)
  , initAndCleanup
  , testWithAsyncDaemon
  , waitUntilEq
  , waitUntilEqSTM
  , waitUntilEqWithTimeout
  , waitUntilEqSTMWithTimeout
  )
  where

import Control.Lens (view, (.~), (&), (^.))
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Network.MQTT.Client (Topic)
import Network.MQTT.Topic (unTopic)
import qualified Service.App as App
import Service.App (Logger (..))
import qualified Service.Daemon as Daemon
import qualified Service.Device as Device
import qualified Service.Env as Env
import Service.Env (Env, LogLevel, appCleanup, config, daemonBroadcast, dbPath, devices,
                    devicesRawJSON, groups, groupsRawJSON)
import qualified Service.Group as Group
import Service.MQTT.Class (MQTTClient (..))
import qualified Service.MQTT.Messages.Daemon as Daemon
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import System.IO.Temp (createTempDirectory)
import qualified Test.Helpers as Helpers
import Test.Helpers (loadTestDevices, loadTestGroups)
import Test.Hspec (Expectation, expectationFailure, shouldBe)
import UnliftIO.Async (race, withAsync)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (bracket)
import UnliftIO.STM (STM, TChan, TVar, atomically, checkSTM, dupTChan, modifyTVar', newTVarIO, writeTVar)

newtype TestMQTTClient = TestMQTTClient (TVar ([Text], HashMap Topic ByteString))

newtype TestLogger = TestLogger (TVar [Text])

instance MQTTClient TestMQTTClient where
  publishMQTT (TestMQTTClient mc) topic msg =
    atomically $ modifyTVar' mc $ \(subs, mqttMsgs) ->
      (subs, M.insert topic msg mqttMsgs)
  subscribeMQTT (TestMQTTClient mc) topic = do
    atomically $ modifyTVar' mc $ \(subs, mqttMsgs) ->
      (subs <> ["subscribe " <> unTopic topic], mqttMsgs)
  unsubscribeMQTT (TestMQTTClient mc) topic = do
    atomically $ modifyTVar' mc $ \(subs, mqttMsgs) ->
      (subs <> ["unsubscribe " <> unTopic topic], mqttMsgs)

instance Logger TestLogger where
  log :: TestLogger -> LogLevel -> Text -> IO ()
  log (TestLogger l) level logStr =
    atomically . modifyTVar' l $ \msgs ->
      msgs <> [ (T.pack . show $ level) <> ": " <> logStr ]

testConfigFilePath :: FilePath
testConfigFilePath = "test/config.dhall"

-- |
-- | Initialization of environment and app start for Integration
-- tests. Meant to be run inside an `around` wrapper in HSpec.
--
-- My dream is for this to go away and to support testing
-- initialization exclusively through configuration alone, insofar as
-- it even needs to be distinct.
--
initAndCleanup :: ((Env TestLogger TestMQTTClient) -> IO ()) -> IO ()
initAndCleanup runTests = bracket
  (do
      env <- Env.initialize testConfigFilePath mkLogger mkMQTTClient

      devices' <- loadTestDevices
      groups' <- loadTestGroups

      let
        devicesTV = env ^. devices
        groupsTV = env ^. groups
        devicesJsonTV = env ^. devicesRawJSON
        groupsJsonTV = env ^. groupsRawJSON

      Daemon.loadResources Device._id devicesTV devices'
      Daemon.loadResources Group._id groupsTV groups'

      devicesJSON <- Helpers.devicesRawJSON
      groupsJSON <- Helpers.groupsRawJSON

      atomically $ do
        writeTVar devicesJsonTV devicesJSON
        writeTVar groupsJsonTV groupsJSON

      -- A unique temporary directory for this invocation's db avoids
      -- path collisions across parallel test runs. Deleted in the
      -- release below.
      tmpDir <- getTemporaryDirectory >>= \tmpParent ->
        createTempDirectory tmpParent "automation-service-test"

      pure (env & config . dbPath .~ tmpDir ++ "/automationState.db", tmpDir)
  )
  (\(env, tmpDir) -> do
      view appCleanup env
      removeDirectoryRecursive tmpDir)
  (\(env, _tmpDir) -> runTests env)

  where
    mkLogger _config = do
      logger <- newTVarIO []
      pure (TestLogger logger, pure ())

    mkMQTTClient _config _loggerVariant _mqttDispatch = do
      fauxMQTTClient <- newTVarIO ([], M.empty)
      pure (TestMQTTClient fauxMQTTClient, pure ())

-- |
-- | Takes a function accepting a bunch of state and returning an
-- Expectation--the actual test block you'd normally place inside
-- `it` basically--and an Env, returns the Expectation. It's
-- intended to be used in a context where the Env is the only
-- argument getting passed in to the function inside of `it`, as a
-- result of using `around` or similar:
--
--
-- @
--
--    daemonSpec :: Spec
--    daemonSpec = do
--      around initAndCleanup $ do
--        it "Starts a thing" $
--          -- env is getting passed in here by the `around
--          -- initAndCleanup` wrapper
--          testWithAsyncDaemon $ \env threadMapTV daemonSnooper -> do
--            let daemonBroadcast' = env ^. daemonBroadcast
--            atomically $ writeTQueue daemonBroadcast' $ Messages.Start Thing
--            -- etc.
--            actual `shouldBe` expected
--
-- @
--
testWithAsyncDaemon
  :: (Logger l, MQTTClient mc)
  =>
    (  (Env l mc)
    -> TVar (Daemon.ThreadMap (App.AutomationService l mc))
    -> TChan Daemon.Message
    -> Expectation
    )
  -> (Env l mc)
  -> Expectation
testWithAsyncDaemon test env = do
  let daemonBroadcast' = env ^. daemonBroadcast
  daemonSnooper <- atomically $ dupTChan daemonBroadcast'
  threadMapTV <- newTVarIO M.empty
  withAsync (App.runAutomationService env $ Daemon.run' threadMapTV) $
    \_async -> test env threadMapTV daemonSnooper

-- |
-- | Helper to repeatedly check a STM action that may take some time
-- to return the expected value. It is assumed that this is being
-- wrapped in a timeout so that it will fail if this expectation is
-- not met, because otherwise this will run forever.
--
-- TODO: this ends up producing fairly unreadable code insofar as it
-- needs to look like a test assertion. Need to improve the syntax so
-- that this ends up looking like it's testing an assertion rather
-- than waiting for something to somehow be equal after executing some
-- incomprehensible STM code.


-- | Poll interval for the IO-based wait helper (10ms between
-- attempts; keeps CPU usage negligible while still responding to
-- state changes within milliseconds).
waitPollIntervalMicros :: Int
waitPollIntervalMicros = 10000

-- | Default timeout value for the waitUntilEq function
defaultWaitUntilEqTimeout :: Int
defaultWaitUntilEqTimeout = (5 * 1000000)

-- | Default timeout value for the waitUntilEqSTM function
defaultWaitUntilEqSTMTimeout :: Int
defaultWaitUntilEqSTMTimeout = (5 * 1000000)

-- | Repeatedly runs the IO action until it yields the expected value,
-- with a default 5-second timeout. Polls at 10ms intervals (the IO
-- action may not read from TVars, so STM blocking is not
-- available). Fails with an informative timeout message if the
-- condition is not met in time.
waitUntilEq :: (Eq a, Show a) => a -> IO a -> Expectation
waitUntilEq expected action =
  waitUntilEqWithTimeout defaultWaitUntilEqTimeout expected action

-- | Repeatedly runs the IO action until it yields the expected value,
-- with the timeout (in microseconds). Polls at 10ms intervals (the
-- IO action may not read from TVars, so STM blocking is not
-- available). Fails with an informative timeout message if the
-- condition is not met in time.
waitUntilEqWithTimeout :: (Eq a, Show a) => Int -> a -> IO a -> Expectation
waitUntilEqWithTimeout waitTimeoutMicros expected action = do
  result <- race (threadDelay waitTimeoutMicros) (waitLoop action)
  case result of
    Left () -> expectationFailure $
      "waitUntilEq: timed out after " <> secondsStr waitTimeoutMicros <> " waiting for: " <> show expected
    Right actual -> actual `shouldBe` expected
  where
    waitLoop act = do
      actual <- act
      if actual == expected
        then pure actual
        else threadDelay waitPollIntervalMicros >> waitLoop act

-- | Repeatedly runs the STM action until it yields the expected
-- value, with a default 5-second timeout. Uses STM's
-- 'checkSTM'/'retry' for zero-CPU blocking: the thread sleeps until
-- any TVar read during the transaction is modified, then the
-- transaction re-executes and the condition is re-checked. Fails
-- with an informative timeout message if the condition is not met
-- in time.
waitUntilEqSTM :: (Eq a, Show a) => a -> STM a -> Expectation
waitUntilEqSTM expected stmAction =
  waitUntilEqSTMWithTimeout defaultWaitUntilEqSTMTimeout expected stmAction

-- | Repeatedly runs the STM action until it yields the expected
-- value, with the timeout (in microseconds). Uses STM's
-- 'checkSTM'/'retry' for zero-CPU blocking: the thread sleeps until
-- any TVar read during the transaction is modified, then the
-- transaction re-executes and the condition is re-checked. Fails
-- with an informative timeout message if the condition is not met
-- in time.
waitUntilEqSTMWithTimeout :: (Eq a, Show a) => Int -> a -> STM a -> Expectation
waitUntilEqSTMWithTimeout waitTimeoutMicros expected stmAction = do
  result <- race (threadDelay waitTimeoutMicros) (atomically $ do
    actual <- stmAction
    checkSTM (actual == expected)
    pure actual)
  case result of
    Left () -> expectationFailure $
      "waitUntilEqSTM: timed out after " <> secondsStr waitTimeoutMicros <> " waiting for: " <> show expected
    Right actual -> actual `shouldBe` expected

secondsStr :: Int -> String
secondsStr waitTimeoutMicros = show $ waitTimeoutMicros `div` 1000000
