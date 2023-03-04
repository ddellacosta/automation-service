module Test.Integration.Service.App.DaemonTestHelpers
  ( initAndCleanup
  , testWithAsyncDaemon
  , waitUntilEq
  )
  where

import Control.Lens ((^.), view)
import qualified Data.Map.Strict as M
import qualified Service.App as App
import qualified Service.App.Daemon as Daemon
import qualified Service.Env as Env
import Service.Env
  ( Env
  , LoggerVariant(QLogger)
  , MQTTClientVariant(..)
  , appCleanup
  , daemonBroadcast
  , devices
  )
import qualified Service.Messages.Daemon as Daemon
import Test.Helpers (loadTestDevices)
import Test.Hspec (Expectation, shouldBe)
import UnliftIO.Async (withAsync)
import UnliftIO.Exception (bracket)
import UnliftIO.STM (STM, TChan, TVar, atomically, dupTChan, newTVarIO)

testConfigFilePath :: FilePath
testConfigFilePath = "test/config.dhall"

-- |
-- | Initialization of environment and app start for Integration
-- tests. Meant to be run inside an `around` wrapper in HSpec
--
initAndCleanup :: (Env -> IO ()) -> IO ()
initAndCleanup runTests = bracket
  (do
      env <- Env.initialize testConfigFilePath mkLogger mkMQTTClient
      devices' <- loadTestDevices
      let devicesTVar = env ^. devices
      Daemon.loadDevices devicesTVar devices'
      pure env
  )
  (view appCleanup)
  runTests
  where
    mkLogger _config = do
      qLogger <- newTVarIO []
      pure (QLogger qLogger, pure ())

    mkMQTTClient _config _loggerVariant _daemonBroadcast = do
      fauxMQTTClient <- newTVarIO M.empty
      pure (TQClient fauxMQTTClient, pure ())

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
waitUntilEq :: (Eq a, Show a) => a -> STM a -> Expectation
waitUntilEq expected action = do
  actual <- atomically action
  if actual == expected
    then actual `shouldBe` expected
    else waitUntilEq expected action
