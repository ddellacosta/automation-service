module Service.Automations.LuaScript
  ( luaAutomation
  ,
  )
where


import Prelude hiding (id, init)

import Control.Lens (view)
import Control.Monad.IO.Unlift (MonadUnliftIO(..), liftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (Value, decode, encode)
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (for_)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
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
import Network.HTTP.Client (httpLbs, newManager, parseRequest, responseBody, responseStatus)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.MQTT.Topic (mkTopic)
import qualified Service.App as App
import Service.App (Logger(..), MonadMQTT(..))
import qualified Service.Automation as Automation
import Service.Automation (Automation(..))
import qualified Service.AutomationName as AutomationName
import Service.Device (Device, DeviceId, toLuaDevice)
import Service.Env
  ( Registrations
  , Env
  , LogLevel(Debug)
  , LoggerVariant(..)
  , MQTTClientVariant(..)
  , config
  , daemonBroadcast
  , deviceRegistrations
  , devices
  , groupRegistrations
  , groups
  , logger
  , luaScriptPath
  , mqttClient
  )
import Service.Group (Group, GroupId, toLuaGroup)
import qualified Service.MQTT.Messages.Daemon as Daemon
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (handle, throwIO)
-- One really nice thing about UnliftIO.STM is that all you need is
-- MonadIO for the most part, so I can use all of these in LuaE.
import UnliftIO.STM
  ( STM
  , TChan
  , TVar
  , atomically
  , dupTChan
  , modifyTVar'
  , newBroadcastTChan
  , readTChan
  , readTVar
  , writeTChan
  )

luaAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => FilePath
  -> UTCTime
  -> Automation m
luaAutomation filepath ts =
  Automation
    { _name = AutomationName.LuaScript filepath
    , _cleanup = mkCleanupAutomation filepath
    , _run = mkRunAutomation filepath
    , _startTime = ts
    }

mkCleanupAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => FilePath
  -> (TChan Automation.Message -> m ())
mkCleanupAutomation filepath = \_broadcastChan -> do
  debug $ "Starting Cleanup: LuaScript " <> T.pack filepath

  logger' <- view logger
  mqttClient' <- view mqttClient
  daemonBroadcast' <- view daemonBroadcast
  devices' <- view devices
  groups' <- view groups

  luaScriptPath' <- view $ config . luaScriptPath
  luaState <- liftIO Lua.newstate

  luaStatusString <- liftIO . Lua.unsafeRunWith luaState $ do
    Lua.openlibs -- load the default Lua packages
    loadDSL filepath logger' mqttClient' daemonBroadcast' devices' groups'
    loadScript luaScriptPath' filepath *> Lua.callTrace 0 0
    callWhenExists "cleanup"

  --
  -- I would mask here but bracket in UnliftIO.Exception uses
  -- uninterruptible masking for the cleanup handler so it's
  -- unnecessary...have to be careful about what we allow in cleanup
  -- functions though. Also see comment in Service.App.Daemon.
  --
  deviceRegs <- view deviceRegistrations
  groupRegs <- view groupRegistrations

  atomically $ do
    updateRegs deviceRegs
    updateRegs groupRegs

  debug $
       "Finished Cleanup: LuaScript "
    <> T.pack filepath
    <> ", status '"
    <> T.pack (show luaStatusString)
    <> "'."

  where
    automationName = AutomationName.LuaScript filepath

    updateRegs :: (Hashable a) => TVar (Registrations a) -> STM ()
    updateRegs regs = modifyTVar' regs $ \regs' ->
      M.foldrWithKey'
        (\idx autos newRegs ->
            case NE.nonEmpty . NE.filter (/= automationName) $ autos of
              Just autos' -> M.insert idx autos' newRegs
              Nothing -> newRegs
        )
        M.empty
        regs'

type StatusMsg = String

mkRunAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => FilePath
  -> (TChan Automation.Message -> m ())
mkRunAutomation filepath = \_broadcastChan -> do
  debug $ "Beginning run of LuaScript " <> T.pack filepath

  logger' <- view logger
  mqttClient' <- view mqttClient
  daemonBroadcast' <- view daemonBroadcast
  devices' <- view devices
  groups' <- view groups

  luaScriptPath' <- view $ config . luaScriptPath
  luaState <- liftIO Lua.newstate

  -- note that the semantics of this follow Lua, not Haskell,
  -- something I didn't understand when I was writing it initially.
  --
  -- TODO think harder about the error handling, in particular make
  -- this failure info available to other parts of the system, in a
  -- more structured data type
  luaStatusString <- handle (\e -> pure . show $ (e :: Lua.Exception)) $
    liftIO . Lua.unsafeRunWith luaState $ do
      Lua.openlibs -- load the default Lua packages
      loadDSL filepath logger' mqttClient' daemonBroadcast' devices' groups'
      -- TODO this needs error handling
      loadScript luaScriptPath' filepath *> Lua.callTrace 0 0

      setupStatus <- maybe "setup function doesn't exist" (const "Ok") <$>
        callWhenExists "setup"
      liftIO $ logDebugMsg' filepath logger' $ "Setup status: " <> setupStatus
      loopAutomation

  debug $
       "LuaScript "
    <> T.pack filepath
    <> " finished with status '"
    <> T.pack (show luaStatusString)
    <> "'."

  where
    -- this is here so we can have an event-loop kinda thing that is
    -- interruptible by AsyncExceptions, vs. doing `while (true) ...`
    -- in Lua which blocks forever.
    loopAutomation :: Lua.LuaE Lua.Exception StatusMsg
    loopAutomation = do
      result <- callWhenExists "loop"
      maybe (pure "loop function doesn't exist.") (const loopAutomation) result

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
    Lua.TypeFunction -> Just <$> Lua.callTrace 0 0
    _ -> pure Nothing

loadDSL
  :: FilePath
  -> LoggerVariant
  -> MQTTClientVariant
  -> TChan Daemon.Message
  -> TVar (HashMap DeviceId Device)
  -> TVar (HashMap GroupId Group)
  -> Lua.LuaE Lua.Exception ()
loadDSL filepath logger' mqttClient' daemonBroadcast' devices' groups' = do
  for_ functions $ \(fn, fnName) ->
    pushDocumentedFunction fn *> Lua.setglobal fnName

  where
    thisAutoName = AutomationName.LuaScript filepath

    functions =
      [ (httpGet, "httpGet")
      , (logDebugMsg, "logDebugMsg")
      , (microSleep, "microSleep")
      , (publish, "publish")
      , (publishString, "publishString")
      , (register, "register")
      , (registerGroup, "registerGroup")
      , (sendMessage, "sendMessage")
      , (sleep, "sleep")
      , (subscribe, "subscribe")
      ]

    httpGet :: DocumentedFunction Lua.Exception
    httpGet =
      defun "httpGet"
      ### (\url -> do
              response <- liftIO $ do
                manager <- newManager tlsManagerSettings
                request <- parseRequest url
                httpLbs request manager
              liftIO . logDebugMsg' filepath logger' $
                "The status code was: " <> (T.pack $ show $ responseStatus response)
              pure $ fromMaybe emptyObject . decode . responseBody $ response
          )
      <#> parameter LM.peekString "string" "url" "url string to get"
      =#> functionResult LA.pushViaJSON "jsonResponse" "jsonResponse"

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

    -- do I really want this?
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
                let
                  registrationMsg = Daemon.RegisterDevice deviceId thisAutoName
                writeTChan daemonBroadcast' $ registrationMsg
                M.lookup deviceId <$> readTVar devices'

              case mDevice of
                Just device ->
                  -- pTraceShow (toLuaDevice device) $
                  pure . toLuaDevice $ device
                -- I REALLY need to think through the error handling here more
                Nothing -> throwIO (Lua.Exception "device doesn't exist")
          )
      <#> parameter LM.peekText "string" "deviceId" "Id for device to register"
      =#> functionResult LA.pushViaJSON "device" "device"

    registerGroup :: DocumentedFunction Lua.Exception
    registerGroup =
      defun "registerGroup"
      ### (\groupId -> do
              mGroup <- atomically $ do
                let
                  registrationMsg = Daemon.RegisterGroup groupId thisAutoName
                writeTChan daemonBroadcast' $ registrationMsg
                M.lookup groupId <$> readTVar groups'

              case mGroup of
                Just group ->
                  -- pTraceShow (toLuaGroup group) $
                  pure . toLuaGroup $ group
                -- I REALLY need to think through the error handling here more
                Nothing -> throwIO (Lua.Exception "group doesn't exist")
          )
      <#> parameter LM.peekIntegral "string" "groupId" "Id for group to register"
      =#> functionResult LA.pushViaJSON "group" "group"

    sleep :: DocumentedFunction Lua.Exception
    sleep =
      defun "sleep"
      ### threadDelay . (* 1000000)
      <#> parameter LM.peekIntegral "int" "seconds" "seconds to delay thread"
      =#> []

    microSleep :: DocumentedFunction Lua.Exception
    microSleep =
      defun "microSleep"
      ### threadDelay
      <#> parameter LM.peekIntegral "int" "microseconds" "microseconds to delay thread"
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
                  Daemon.Subscribe thisAutoName (mkTopic topic) automationBroadcastChan
                pure listenerChan
              liftIO . mkListenerFn $ listenerChan'
          )
      <#> parameter LM.peekText "string" "topic" "topic to subscribe to"
      =#> functionResult pushDocumentedFunction "function" "fn"

    mkListenerFn :: TChan Value -> IO (DocumentedFunction Lua.Exception)
    mkListenerFn listenerChan = do
      fnName' <- liftIO UUID.nextRandom
      let fnName = BS.pack . UUID.toString $ fnName'
      pure $
        defun (Lua.Name fnName)
        ### (atomically . readTChan $ listenerChan)
        =#> functionResult LA.pushViaJSON "msg" "incoming data from subscribed topic"


-- this is here because it's useful for throwing into other
-- Lua-Monad functions during debugging
logDebugMsg' :: FilePath -> LoggerVariant -> Text -> IO ()
logDebugMsg' filepath logger' msg =
  App.logWithVariant logger' Debug (T.pack filepath <> ": " <> msg)
