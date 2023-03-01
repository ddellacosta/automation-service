module Test.Integration.Service.App.DaemonTestHelpers
  ( blockUntilNextEventLoop
  , initAndCleanup
  , lookupOrFail
  , testWithAsyncDaemon
  )
  where

import Control.Lens ((^.), view)
import qualified Data.Map.Strict as M
import Control.Monad (void)
import qualified Service.App as App
import Service.App (AutomationService)
import qualified Service.App.Daemon as Daemon
import Service.App.DaemonState (DaemonState, ServerResponse, initDaemonState)
import qualified Service.Env as Env
import Service.Env
  ( Config
  , Env
  , LoggerVariant(QLogger)
  , MQTTClientVariant(..)
  , appCleanup
  , messageQueue
  )
import qualified Service.Messages.Daemon as Daemon
import Test.Hspec (Expectation, expectationFailure)
import UnliftIO.Async (withAsync)
import UnliftIO.Exception (bracket)
import UnliftIO.STM (TQueue, atomically, newTQueueIO, newTVarIO, readTQueue)

testConfigFilePath :: FilePath
testConfigFilePath = "test/config.dhall"

mkLogger :: Config -> IO (LoggerVariant, IO ())
mkLogger _config = do
  qLogger <- newTVarIO []
  pure (QLogger qLogger, pure ())

initAndCleanup :: (Env -> IO ()) -> IO ()
initAndCleanup runTests = bracket
  (Env.initialize testConfigFilePath mkLogger mkMQTTClient)
  (view appCleanup)
  runTests
  where
    mkMQTTClient _ _ _ = do
      fauxMQTTClient <- newTVarIO M.empty
      pure (TQClient fauxMQTTClient, pure ())

--
-- This is part of a hack. In Daemon, at the end of every message loop it
-- sends a message to ServerResponse noting the loop has completed. I
-- introduced ServerResponse to Daemon specifically so I could write
-- integration-ish tests at this level of abstraction, and it's ugly,
-- but it's not clear to me how else I can test effectively without
-- this hack.
--
blockUntilNextEventLoop :: TQueue ServerResponse -> IO ()
blockUntilNextEventLoop = void . atomically . readTQueue

-- |
-- | Helper to elide the Maybe check in testing the value of a Map
-- | lookup when the value is expected to be present. Takes an
-- | alternative failure message to give when a Nothing value is
-- | received unexpectedly.
-- |
lookupOrFail :: (Ord k) => String -> k -> M.Map k v -> (v -> Expectation) -> Expectation
lookupOrFail msg k m assertion =
  case M.lookup k m of
    Just v -> assertion v
    Nothing -> expectationFailure msg

-- |
-- | Takes a function accepting a bunch of state and returning an
-- | Expectation--the actual test block you'd normally place inside
-- | `it` basically--and an Env, returns the Expectation. It's
-- | intended to be used in a context where the Env is the only
-- | argument getting passed in to the function inside of `it`, as a
-- | result of using `around` or similar:
-- |
--
-- @
--
--    daemonSpec :: Spec
--    daemonSpec = do
--      around initAndCleanup $ do
--        it "Starts a thing" $
--          testWithAsyncDaemon $ \daemonState messageQueue' responseQueue -> do
--            atomically $ writeTQueue messageQueue' $ Messages.Start Thing
--            -- no Show instance for ActionEntry pair
--            (void . M.lookup Thing) threadMap' `shouldBe` Just ()
--
-- @
-- |
testWithAsyncDaemon
  ::
    (  Env
    -> DaemonState AutomationService
    -> TQueue Daemon.Message
    -> TQueue ServerResponse
    -> Expectation
    )
  -> Env
  -> Expectation
testWithAsyncDaemon test env = do
  let messageQueue' = env ^. messageQueue
  responseQueue <- newTQueueIO
  daemonState <- initDaemonState
  withAsync (App.runAutomationService env $ Daemon.run' daemonState responseQueue) $
    \_async -> test env daemonState messageQueue' responseQueue
