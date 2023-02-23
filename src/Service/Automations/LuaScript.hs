module Service.Automations.LuaScript
  ( mkLuaAutomation
  ,
  )
where

import Prelude hiding (id, init)

-- import GHC.Exception (SomeException)

import Control.Lens (view)
import Control.Lens.Unsound (lensProduct)
import Control.Monad.Reader (MonadReader)
import Control.Monad.IO.Unlift (MonadUnliftIO(..), liftIO)
import qualified Data.Text as T
import Data.Text (Text)
import qualified HsLua.Core as Lua
import HsLua.Core
  ( Lua
  , LuaE
  , NumResults(NumResults)
  , dostring
  , loadfile
  , openlibs
  , pushHaskellFunction
  , setglobal
  )
import qualified Service.App as App
import Service.App (Logger(..), MonadMQTT(..))
import Service.App.DaemonState ()
import Service.Automation (Automation(..), Message(..))
import qualified Service.AutomationName as AutomationName
import qualified Service.Device as Device
import Service.Env (Env', LogLevel(Debug), config, logger, luaScriptPath, messageQueue)
import qualified Service.Messages.Daemon as Daemon
import System.Log.FastLogger (TimedFastLogger)
import UnliftIO.STM (TChan, TQueue, atomically, writeTQueue)

mkLuaAutomation
  :: (Logger m, MonadMQTT m, MonadReader (Env' logger mqttClient) m, MonadUnliftIO m)
  => FilePath
  -> Automation m
mkLuaAutomation filePath =
  Automation
    { _name = AutomationName.LuaScript
    -- these two will need to be dynamically definable and resettable
    -- within this Automation if this is going to make sense for lua
    , _devices = []
    , _wantsFullControlOver = []
    , _cleanup = cleanupAutomation
    , _run = mkRunAutomation filePath
    }

cleanupAutomation
  :: (Logger m, MonadMQTT m, MonadReader (Env' logger mqttClient) m, MonadUnliftIO m)
  => TChan Message
  -> m ()
cleanupAutomation _broadcastChan = do
  pure ()

-- class LuaLogger a where
--   log :: TimedFastLogger logger
-- 
-- instance LuaLogger (Lua a)
--   log = flip App.log Debug

mkRunAutomation
  :: (Logger m, MonadMQTT m, MonadReader (Env' logger mqttClient) m, MonadUnliftIO m)
  => FilePath
  -> TChan Message
  -> m ()
mkRunAutomation filePath = \_broadcastChan -> do
  debug $ "Initializing a Lua Script Automation for script " <> T.pack filePath
  (logger', messageQueue') <- view $ lensProduct logger messageQueue
  luaScriptPath' <- view $ config . luaScriptPath

  luaStatus <- liftIO $ Lua.run $ do
    openlibs -- load the default Lua packages
    pushHaskellFunction (publish messageQueue')
--    pushHaskellFunction (logDebugMsg logger')
    setglobal "publish"
--    setglobal "logDebugMsg"
    loadfile $ luaScriptPath' <> filePath
  debug $ "Lua finished with status '" <> T.pack (show luaStatus) <> "'."

  where
--    logDebugMsg :: forall logger. logger -> Lua NumResults
--    logDebugMsg logger' = do 
--      log logger' "hey Lua"
--      pure $ NumResults 0

    publish :: TQueue Daemon.Message -> Lua NumResults
    publish messageQueue' = do
      liftIO $ putStrLn "Does this work?"
      liftIO $
        atomically $
          writeTQueue messageQueue' (Daemon.Start AutomationName.Gold)
      -- pushinteger 23
      pure $ NumResults 0
