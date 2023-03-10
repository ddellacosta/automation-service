module Service.Automations.LuaScript
  ( luaAutomation
  ,
  )
where


import Prelude hiding (id, init)

import Control.Lens (view)
import Control.Monad (void)
import Control.Monad.IO.Unlift (MonadUnliftIO(..), liftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (Value, decode, encode, object)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (for_)
import qualified Data.List.NonEmpty as NE
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
import Service.Device (Device, DeviceId, toLuaDevice)
import Service.Env
  ( Env
  , LogLevel(Debug)
  , LoggerVariant(..)
  , MQTTClientVariant(..)
  , config
  , daemonBroadcast
  , deviceRegistrations
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
  , modifyTVar'
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
    loadScript luaScriptPath' filepath *> Lua.callTrace 0 0
    callWhenExists "cleanup"

  --
  -- I would mask here but bracket in UnliftIO.Exception uses
  -- uninterruptible masking for the cleanup handler so it's
  -- unnecessary...have to be careful about what we allow in cleanup
  -- functions though. Also see comment in Service.App.Daemon.
  --
  -- atomically $ modifyTVar' deviceRegs updateDeviceAutomations
  deviceRegs <- view deviceRegistrations
  atomically $ modifyTVar' deviceRegs $ \deviceRegs' ->
    M.foldrWithKey'
      (\di autos newRegs ->
          case NE.nonEmpty . NE.filter (/= automationName) $ autos of
            Just autos' -> M.insert di autos' newRegs
            Nothing -> newRegs
      )
      M.empty
      deviceRegs'

  debug $ "Lua cleanup finished with status '" <> T.pack (show luaStatusString) <> "'."

  where
    automationName = (AutomationName.LuaScript filepath)

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
    loadScript luaScriptPath' filepath *> Lua.callTrace 0 0

    void $ callWhenExists "setup"
    loopAutomation

  debug $ "Lua loopAutomation finished with status '" <> T.pack (show luaStatusString) <> "'."

  where
    -- this is here so we can have an event-loop kinda thing that is
    -- interruptible by AsyncExceptions, vs. doing `while (true) ...`
    -- in Lua which blocks forever.
    loopAutomation :: Lua.LuaE Lua.Exception ()
    loopAutomation = do
      result <- callWhenExists "loop"
      maybe (pure ()) (const loopAutomation) result

loadScript :: FilePath -> FilePath -> Lua.LuaE Lua.Exception Lua.Status
loadScript luaScriptPath' filepath =
  Lua.loadfile $ luaScriptPath' <> filepath <> ".lua"

-- the Maybe here is a little bit hacky, just to let me match on
-- it in loopAutomation in mkRunAutomation so that doesn't run
-- endlessly. I would like some better error handling for the
-- callTrace too:
callWhenExists :: Lua.Name -> Lua.LuaE Lua.Exception (Maybe ())
callWhenExists fnName = do
  setupFn <- Lua.getglobal fnName
  case setupFn of
    Lua.TypeFunction -> Just <$> Lua.callTrace 0 1
    _ -> pure Nothing

loadDSL
  :: FilePath
  -> LoggerVariant
  -> MQTTClientVariant
  -> TChan Daemon.Message
  -> TVar (Map DeviceId Device)
  -> Lua.LuaE Lua.Exception ()
loadDSL filepath logger' mqttClient' daemonBroadcast' devices' = do
  for_ functions $ \(fn, fnName) ->
    pushDocumentedFunction fn *> Lua.setglobal fnName

  where
    functions =
      [ (logDebugMsg, "logDebugMsg")
      , (publish, "publish")
      , (publishString, "publishString")
      , (register, "register")
      , (sendMessage, "sendMessage")
      , (sleep, "sleep")
      , (subscribe, "subscribe")
      ]

    logDebugMsg :: DocumentedFunction Lua.Exception
    logDebugMsg =
      defun "logDebugMsg"
      -- I'll be honest with you, I have no idea what types any of
      -- these combinators are, other than having a bunch of LuaE in
      -- there
      ### liftIO . logDebugMsg' filepath logger'
      <#> parameter LM.peekText "string" "logString" "string to log"
      =#> []

    publishImpl :: Text -> ByteString -> Lua.LuaE Lua.Exception ()
    publishImpl topic msg = liftIO $
      App.publish (fromMaybe "" $ mkTopic topic) msg mqttClient'

    publish :: DocumentedFunction Lua.Exception
    publish =
      defun "publish"
      ### (\topic -> publishImpl topic . encode)
      <#> parameter LM.peekText "string" "topic" "topic for device"
      <#> parameter LA.peekValue "table" "jsonMsg" "MQTT JSON string msg to send"
      =#> []

    publishString :: DocumentedFunction Lua.Exception
    publishString =
      defun "publishString"
      ### publishImpl
      <#> parameter LM.peekText "string" "topic" "topic for device"
      <#> parameter LM.peekLazyByteString "string" "msg" "MQTT JSON string msg to send"
      =#> []

    register :: DocumentedFunction Lua.Exception
    register =
      defun "register"
      ### (\deviceId -> do
              mDevice <- atomically $ do
                let registrationMsg =
                      Daemon.Register deviceId (AutomationName.LuaScript filepath)
                writeTChan daemonBroadcast' $ registrationMsg
                M.lookup deviceId <$> readTVar devices'

              case mDevice of
                Just device -> pure . toLuaDevice $ device
                -- I REALLY need to think through the error handling here more
                Nothing -> throwIO (Lua.Exception "device doesn't exist")
          )
      <#> parameter LM.peekText "string" "deviceId" "Id for device to register"
      =#> functionResult LA.pushViaJSON "device" "device"

    sleep :: DocumentedFunction Lua.Exception
    sleep =
      defun "sleep"
      ### threadDelay . (* 1000000)
      <#> parameter LM.peekIntegral "int" "seconds" "seconds to delay thread"
      =#> []

    sendMessage :: DocumentedFunction Lua.Exception
    sendMessage =
      defun "sendMessage"
      ### (\msg -> fromMaybe (pure ()) $
             atomically <$> writeTChan daemonBroadcast' <$> decode msg
          )
      <#> parameter LM.peekLazyByteString "string" "message" "string to log"
      =#> []

    subscribe :: DocumentedFunction Lua.Exception
    subscribe =
      defun "subscribe"
      ### (\topic -> do
              listenerChan' <- atomically $ do
                automationBroadcastChan <- newBroadcastTChan
                listenerChan <- dupTChan $ automationBroadcastChan
                writeTChan daemonBroadcast' $
                  Daemon.Subscribe (mkTopic topic) automationBroadcastChan
                pure listenerChan
              liftIO . mkListenerFn $ listenerChan'

              -- listenerChan <- atomically . dupTChan $ automationBroadcastChan
              -- automationBroadcastChan <- newBroadcastTChan
              -- atomically . writeTChan daemonBroadcast' $
              --   Daemon.Subscribe (mkTopic topic) automationBroadcastChan
              -- liftIO $ mkListenerFn listenerChan
          )
      <#> parameter LM.peekText "string" "topic" "topic to subscribe to"
      =#> functionResult pushDocumentedFunction "function" "fn"

    mkListenerFn :: TChan Value -> IO (DocumentedFunction Lua.Exception)
    mkListenerFn listenerChan = do
      fnName' <- liftIO UUID.nextRandom
      let fnName = BS.pack . UUID.toString $ fnName'
      pure $
        defun (Lua.Name fnName)
        -- we don't want this to block, it prevents LuaScript threads
        -- from being interruptible
        ### (atomically $ do
                tryReadTChan listenerChan >>= \mMsg ->
                  -- TODO: make the default response better
                  pure $ fromMaybe (object [("msg", "NoMsg")]) mMsg
            )
        =#> functionResult LA.pushViaJSON "msg" "incoming data from subscribed topic"


-- this is here because it's useful for throwing into other
-- Lua-Monad functions during debugging
logDebugMsg' :: FilePath -> LoggerVariant -> Text -> IO ()
logDebugMsg' filepath logger' msg =
  App.logWithVariant logger' Debug (T.pack filepath <> ": " <> msg)
