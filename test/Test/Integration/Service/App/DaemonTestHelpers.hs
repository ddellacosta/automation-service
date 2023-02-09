module Test.Integration.Service.App.DaemonTestHelpers
  ( daemonStateFixture
  , findAction
  , initAndCleanup
  )
  where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as M
import Test.Integration.TestApp (initEnv)
import Service.App.DaemonState (DaemonState(..), DeviceMap, ThreadMap)
import Service.Action (Action, ActionFor(ActionFor), nullAction)
import Service.ActionName (ActionName(..))
import Service.Device (DeviceId(TestDevice))
import Service.Env (Env, appCleanup)
import UnliftIO.Exception (bracket)
import UnliftIO.STM (atomically, dupTChan, newBroadcastTChanIO, newTVarIO)


daemonStateFixture :: IO (DaemonState m)
daemonStateFixture = do
  broadcastChan <- newBroadcastTChanIO
  serverChan <- atomically $ dupTChan broadcastChan
  threadMap <- liftIO $ newTVarIO (M.empty :: ThreadMap m)
  deviceMap <- liftIO $ newTVarIO (M.empty :: DeviceMap)
  pure $ DaemonState threadMap deviceMap broadcastChan serverChan

findAction :: (Applicative m) => ActionName -> Action m
findAction = \case
  Test -> ActionFor Test [TestDevice] [TestDevice] noop noop
  _ -> nullAction
  where
    noop = const $ pure ()

initAndCleanup :: ((Env, DaemonState m) -> IO ()) -> IO ()
initAndCleanup runTests =
  bracket initialize (\(env, _) -> env ^. appCleanup) runTests
  where
    initialize = do
      env <- initEnv
      daemonState <- daemonStateFixture
      pure (env, daemonState)
