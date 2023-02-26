{-# LANGUAGE TemplateHaskell #-}

module Service.Automations.LuaScript
  ( mkLuaAutomation
  ,
  )
where

import Prelude hiding (id, init)

-- import GHC.Exception (SomeException)

import Control.Lens (makeFieldsNoPrefix, view)
import Control.Lens.Unsound (lensProduct)
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO(..), liftIO)
import qualified Data.Text as T
import Data.Text (Text)
import qualified HsLua.Core as Lua
import HsLua.Core
  ( Lua
  , LuaE
  , NumResults(NumResults)
  , dofile
  , dostring
  , openlibs
  , pushHaskellFunction
  , setglobal
  )
import HsLua.Marshalling
import HsLua.Packaging.Function
import HsLua.Packaging.Convenience
import qualified Service.App as App
import Service.App (Logger(..), MonadMQTT(..))
import Service.App.DaemonState ()
import Service.Automation (Automation(..), Message(..))
import qualified Service.AutomationName as AutomationName
import qualified Service.Device as Device
import Service.Env
  ( Env
  , LoggerVariant(..)
  , LogLevel(Debug)
  , MQTTClientVariant(..)
  , config
  , logger
  , luaScriptPath
  , messageQueue
  , mqttClient
  )
import qualified Service.Messages.Daemon as Daemon
import System.Log.FastLogger (TimedFastLogger)
import UnliftIO.STM (TChan, TQueue, atomically, writeTQueue)

mkLuaAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
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
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => TChan Message
  -> m ()
cleanupAutomation _broadcastChan = do
  pure ()

mkRunAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => FilePath
  -> TChan Message
  -> m ()
mkRunAutomation filePath = \_broadcastChan -> do
  debug $ "Initializing a Lua Script Automation for script " <> T.pack filePath
  (logger', mqttClient') <- view $ lensProduct logger mqttClient
  messageQueue' <- view messageQueue
  luaScriptPath' <- view $ config . luaScriptPath

  luaStatus <- liftIO $ Lua.run $ do
    openlibs -- load the default Lua packages
    pushDocumentedFunction (publish messageQueue')
    setglobal "publish"
    pushDocumentedFunction (logDebugMsg logger')
    setglobal "logDebugMsg"
    dofile $ luaScriptPath' <> filePath
  debug $ "Lua finished with status '" <> T.pack (show luaStatus) <> "'."

  where
    logDebugMsg :: LoggerVariant -> DocumentedFunction Lua.Exception
    logDebugMsg logger' =
      defun "logDebugMsg"
      ### (\ls -> case logger' of
             TFLogger tfLogger -> liftIO $ App.log tfLogger Debug ls
             TQLogger _textLogger -> pure ()
          )
      <#> parameter peekText "string" "logString" "string to log"
      =#> []

    publish :: TQueue Daemon.Message -> DocumentedFunction Lua.Exception
    publish messageQueue' =
      defun "publish"
      ### (atomically $ writeTQueue messageQueue' (Daemon.Start AutomationName.Trinity))
      =#> []
