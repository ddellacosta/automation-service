module Test.Integration.Service.App.DaemonTestHelpers
  ( findAction
  , initAndCleanup
  )
  where

import Control.Lens (view)
import Service.Action (Action(Action), nullAction)
import Service.ActionName (ActionName(..))
import Service.Device (DeviceId(TestDevice))
import Service.Env (appCleanup)
import Test.Integration.TestApp (Env, initEnv)
import UnliftIO.Exception (bracket)

findAction :: (Applicative m) => ActionName -> Action m
findAction = \case
  Test -> Action Test [TestDevice] [TestDevice] noop noop
  _ -> nullAction
  where
    noop = const $ pure ()

initAndCleanup :: (Env -> IO ()) -> IO ()
initAndCleanup runTests =
  bracket initEnv (view appCleanup) runTests
