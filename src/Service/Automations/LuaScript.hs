module Service.Automations.LuaScript
  ( luaAutomation
  ,
  )
where

import Prelude hiding (id, init)

import Control.Lens (view)
import Control.Monad.IO.Unlift (MonadUnliftIO(..), liftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (Value, decode, encode, object)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (forM_)
import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified HsLua.Aeson as LA
import qualified HsLua.Core as Lua
import qualified HsLua.Marshalling as LM
import HsLua.Packaging.Function
  ( DocumentedFunction
  , (<#>), (###), (=#>)
  , defun
  , functionResult
  , parameter
  , pushDocumentedFunction
  )
import Network.MQTT.Topic (mkTopic)
import qualified Service.App as App
import Service.App (Logger(..), MonadMQTT(..))
import qualified Service.Automation as Automation
import Service.Automation (Automation(..))
import qualified Service.AutomationName as AutomationName
import Service.Device (Device, DeviceId)
import Service.Env
  ( Env
  , LogLevel(Debug)
  , LoggerVariant(..)
  , MQTTClientVariant(..)
  , config
  , daemonBroadcast
  , devices
  , logger
  , luaScriptPath
  , mqttClient
  )
import qualified Service.Messages.Daemon as Daemon
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwIO)
import UnliftIO.STM
  ( TChan
  , TVar
  , atomically
  , dupTChan
  , newBroadcastTChan
  , readTVar
  , tryReadTChan
  , writeTChan
  )

luaAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => FilePath
  -> Automation m
luaAutomation filepath =
  Automation
    { _name = AutomationName.LuaScript filepath
    , _cleanup = mkCleanupAutomation filepath
    , _run = mkRunAutomation filepath
    }

mkCleanupAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => FilePath
  -> TChan Automation.Message
  -> m ()
mkCleanupAutomation filepath = \_broadcastChan -> do
  debug $ "Cleaning up LuaScript Automation for script " <> T.pack filepath

  logger' <- view logger
  mqttClient' <- view mqttClient
  daemonBroadcast' <- view daemonBroadcast
  devices' <- view devices

  luaScriptPath' <- view $ config . luaScriptPath
  luaState <- liftIO Lua.newstate

  luaStatusString <- liftIO . Lua.unsafeRunWith luaState $ do
    Lua.openlibs -- load the default Lua packages
    loadDSL filepath logger' mqttClient' daemonBroadcast' devices'
    (Lua.loadfile $ luaScriptPath' <> filepath) *> Lua.callTrace 0 0
    -- should probably test that this actually exists
    _ <- Lua.getglobal "cleanup"
    Lua.callTrace 0 1

  debug $ "Lua cleanup finished with status '" <> T.pack (show luaStatusString) <> "'."

mkRunAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => FilePath
  -> TChan Automation.Message
  -> m ()
mkRunAutomation filepath = \_broadcastChan -> do
  debug $ "Initializing a LuaScript Automation for script " <> T.pack filepath

  logger' <- view logger
  mqttClient' <- view mqttClient
  daemonBroadcast' <- view daemonBroadcast
  devices' <- view devices

  luaScriptPath' <- view $ config . luaScriptPath
  luaState <- liftIO Lua.newstate

  luaStatusString <- liftIO . Lua.unsafeRunWith luaState $ do
    Lua.openlibs -- load the default Lua packages
    loadDSL filepath logger' mqttClient' daemonBroadcast' devices'
    -- apparently you need call/callTrace after loadfile to execute
    -- the chunk (whatever this actually means, it's not clear to
    -- me--is it running the script?), otherwise you can't load a
    -- function up and run it. I wish this was better documented so I
    -- didn't have to bang my head against reference docs without a
    -- clue for an hour before spelunking in the HsLua test code
    -- happened to yield results (see
    -- hslua-core/test/HsLua/CoreTests.hs)
    --
    -- ...also TODO this needs error handling
    (Lua.loadfile $ luaScriptPath' <> filepath) *> Lua.callTrace 0 0
    loopAutomation

  debug $ "Lua loopAutomation finished with status '" <> T.pack (show luaStatusString) <> "'."

  where
    -- this is here so we can have an event-loop kinda thing that is
    -- interruptible by AsyncExceptions, vs. doing `while (true) ...`
    -- in Lua which blocks forever.
    loopAutomation :: Lua.LuaE Lua.Exception ()
    loopAutomation = do
      -- TODO needs error handling
      _ <- Lua.getglobal "loopAutomation"
      Lua.callTrace 0 1
      loopAutomation

loadDSL
  :: FilePath
  -> LoggerVariant
  -> MQTTClientVariant
  -> TChan Daemon.Message
  -> TVar (Map DeviceId Device)
  -> Lua.LuaE Lua.Exception ()
loadDSL filepath logger' mqttClient' daemonBroadcast' devices' = do
  forM_ functions $ \(fn, fnName) ->
    pushDocumentedFunction fn *> Lua.setglobal fnName

  where
    functions =
      [ (logDebugMsg, "logDebugMsg")
      , (publishJSON, "publishJSON")
      , (register, "register")
      , (sendMessage, "sendMessage")
      , (sleep, "sleep")
      , (subscribe, "subscribe")
      , (publish, "publish")
      ]

    logDebugMsg :: DocumentedFunction Lua.Exception
    logDebugMsg =
      defun "logDebugMsg"
      ### liftIO . logDebugMsg' filepath logger'
      <#> parameter LM.peekText "string" "logString" "string to log"
      =#> []

    register :: DocumentedFunction Lua.Exception
    register =
      defun "register"
      ### (\deviceId -> do
              let registrationMsg =
                    Daemon.Register deviceId (AutomationName.LuaScript filepath)
              atomically . writeTChan daemonBroadcast' $ registrationMsg
              deviceMap <- atomically . readTVar $ devices'
              case M.lookup deviceId deviceMap of
                Just device -> do
                  pure device
                -- I REALLY need to think through the error handling here more
                Nothing -> throwIO (Lua.Exception "device doesn't exist")
          )
      <#> parameter LM.peekText "string" "deviceId" "Id for device to register"
      =#> functionResult LA.pushViaJSON "device" "device"

    publishImpl :: Text -> ByteString -> Lua.LuaE Lua.Exception ()
    publishImpl topic msg = liftIO $
      App.publish (fromMaybe "" $ mkTopic topic) msg mqttClient'

    mkListenerFn :: TChan Value -> IO (DocumentedFunction Lua.Exception)
    mkListenerFn incomingChan = do
      fnName' <- liftIO UUID.nextRandom
      let fnName = BS.pack . UUID.toString $ fnName'
      pure $
        defun (Lua.Name fnName)
        -- we don't want this to block, it prevents LuaScript threads
        -- from being interruptible
        ### (atomically $ do
                tryReadTChan incomingChan >>= \mMsg ->
                  -- TODO: make the default response better
                  pure $ fromMaybe (object [("msg", "NoMsg")]) mMsg
            )
        =#> functionResult LA.pushViaJSON "incoming" "incoming data from subscribed topic"

    subscribe :: DocumentedFunction Lua.Exception
    subscribe =
      defun "subscribe"
      ### (\topic -> do
              automationBroadcastChan <- atomically newBroadcastTChan
              listenerChan <- atomically . dupTChan $ automationBroadcastChan
              let subscribeMsg = Daemon.Subscribe (mkTopic topic) automationBroadcastChan
              atomically . writeTChan daemonBroadcast' $ subscribeMsg
              liftIO $ mkListenerFn listenerChan
          )
      <#> parameter LM.peekText "string" "topic" "topic to subscribe to"
      =#> functionResult pushDocumentedFunction "function" "fn"

    publish :: DocumentedFunction Lua.Exception
    publish =
      defun "publish"
      ### publishImpl
      <#> parameter LM.peekText "string" "topic" "topic for device"
      <#> parameter LM.peekLazyByteString "string" "msg" "MQTT JSON string msg to send"
      =#> []

    publishJSON :: DocumentedFunction Lua.Exception
    publishJSON =
      defun "publish"
      ### (\topic -> publishImpl topic . encode)
      <#> parameter LM.peekText "string" "topic" "topic for device"
      <#> parameter LA.peekValue "table" "jsonMsg" "MQTT JSON string msg to send"
      =#> []

    sendMessage :: DocumentedFunction Lua.Exception
    sendMessage =
      defun "publish"
      ### (\msg -> fromMaybe (pure ()) $
             atomically <$> writeTChan daemonBroadcast' <$> decode msg
          )
      <#> parameter LM.peekLazyByteString "string" "message" "string to log"
      =#> []

    sleep :: DocumentedFunction Lua.Exception
    sleep =
      defun "sleep"
      ### threadDelay . (* 1000000)
      <#> parameter LM.peekIntegral "int" "seconds" "seconds to delay thread"
      =#> []

-- this is here because it's useful for throwing into other
-- Lua-Monad functions during debugging
logDebugMsg' :: FilePath -> LoggerVariant -> Text -> IO ()
logDebugMsg' filepath logger' msg =
  App.logWithVariant logger' Debug (T.pack filepath <> ": " <> msg)
