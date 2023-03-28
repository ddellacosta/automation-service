module Test.Integration.Service.DaemonTestHelpers
  ( initAndCleanup
  , testWithAsyncDaemon
  , waitUntilEq
  , waitUntilEqSTM
  )
  where

import Control.Lens ((&), (%~), (^.), view)
import qualified Data.HashMap.Strict as M
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Service.App as App
import qualified Service.Daemon as Daemon
import qualified Service.Device as Device
import qualified Service.Env as Env
import Service.Env
  ( Env
  , LoggerVariant(QLogger)
  , MQTTClientVariant(..)
  , appCleanup
  , config
  , daemonBroadcast
  , dbPath
  , devices
  , groups
  )
import qualified Service.Group as Group
import qualified Service.MQTT.Messages.Daemon as Daemon
import Test.Helpers (loadTestDevices, loadTestGroups)
import Test.Hspec (Expectation, shouldBe)
import UnliftIO.Async (withAsync)
import UnliftIO.Exception (bracket)
import UnliftIO.STM (STM, TChan, TVar, atomically, dupTChan, newTVarIO)

testConfigFilePath :: FilePath
testConfigFilePath = "test/config.dhall"

-- |
-- | Initialization of environment and app start for Integration
-- tests. Meant to be run inside an `around` wrapper in HSpec.
--
initAndCleanup :: (Env -> IO ()) -> IO ()
initAndCleanup runTests = bracket
  (do
      env <- Env.initialize testConfigFilePath mkLogger mkMQTTClient

      devices' <- loadTestDevices
      groups' <- loadTestGroups

      let
        devicesTVar = env ^. devices
        groupsTVar = env ^. groups

      Daemon.loadResources Device._id devicesTVar devices'
      Daemon.loadResources Group._id groupsTVar groups'

      uuid <- UUID.nextRandom
      pure $
        env & config . dbPath %~ \dp -> dp <> "-" <> UUID.toString uuid <> ".db"
  )
  (view appCleanup)
  runTests

  where
    mkLogger _config = do
      qLogger <- newTVarIO []
      pure (QLogger qLogger, pure ())

    mkMQTTClient _config _loggerVariant _mqttDispatch = do
      fauxMQTTClient <- newTVarIO M.empty
      pure (TVClient fauxMQTTClient, pure ())

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
  ::
    (  Env
    -> TVar (Daemon.ThreadMap App.AutomationService)
    -> TChan Daemon.Message
    -> Expectation
    )
  -> Env
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
--
waitUntilEq :: (Eq a, Show a) => a -> IO a -> Expectation
waitUntilEq expected action = do
  actual <- action
  if actual == expected
    then actual `shouldBe` expected
    else waitUntilEq expected action

waitUntilEqSTM :: (Eq a, Show a) => a -> STM a -> Expectation
waitUntilEqSTM e = waitUntilEq e . atomically