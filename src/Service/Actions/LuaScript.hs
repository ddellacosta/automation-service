module Service.Actions.LuaScript
  ( mkLuaAction
  ,
  )
where

import Prelude hiding (id, init)

-- import GHC.Exception (SomeException)

import Control.Lens (view)
import Control.Monad.Reader (MonadReader)
import Control.Monad.IO.Unlift (MonadUnliftIO(..), liftIO)
import qualified Data.Text as T
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
import Service.App (Logger(..), MonadMQTT(..))
import Service.App.DaemonState ()
import Service.Action (Action(..), Message(..))
import qualified Service.ActionName as ActionName
import qualified Service.Device as Device
import Service.Env (Env', config, luaScriptPath, messageQueue)
import qualified Service.Messages.Action as Messages
import UnliftIO.STM (TChan, TQueue, atomically, writeTQueue)

mkLuaAction
  :: (Logger m, MonadMQTT m, MonadReader (Env' logger mqttClient) m, MonadUnliftIO m)
  => FilePath
  -> Action m
mkLuaAction filePath =
  Action
    { _name = ActionName.LuaScript
    -- these two will need to be dynamically definable and resettable
    -- within this Action if this is going to make sense for lua
    , _devices = [Device.GledoptoGLC007P_1]
    , _wantsFullControlOver = [Device.GledoptoGLC007P_1]
    , _cleanup = cleanupAction
    , _run = mkRunAction filePath
    }

cleanupAction
  :: (Logger m, MonadMQTT m, MonadReader (Env' logger mqttClient) m, MonadUnliftIO m)
  => TChan Message
  -> m ()
cleanupAction _broadcastChan = do
  pure ()

mkRunAction
  :: (Logger m, MonadMQTT m, MonadReader (Env' logger mqttClient) m, MonadUnliftIO m)
  => FilePath
  -> TChan Message
  -> m ()
mkRunAction filePath = \_broadcastChan -> do
  debug $ "Initializing a Lua Script Action for script " <> T.pack filePath
  messageQueue' <- view messageQueue
  luaScriptPath' <- view $ config . luaScriptPath

  luaStatus <- liftIO $ Lua.run $ do
    openlibs -- load the default Lua packages
    pushHaskellFunction (publish messageQueue')
    setglobal "publish"
    loadfile $ luaScriptPath' <> filePath
  debug $ "Lua finished with status '" <> T.pack (show luaStatus) <> "'."

  where
    -- debug = App.log logger' Debug

    publish :: TQueue Messages.Action -> Lua NumResults
    publish messageQueue' = do
      liftIO $ putStrLn "Does this work?"
      liftIO $
        atomically $
          writeTQueue messageQueue' (Messages.Start ActionName.Gold)
      -- pushinteger 23
      pure $ NumResults 1
