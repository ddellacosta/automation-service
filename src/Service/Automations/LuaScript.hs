{-# LANGUAGE TemplateHaskell #-}

module Service.Automations.LuaScript
  ( luaAutomation
  ,
  )
where

import Prelude hiding (id, init)

import Control.Lens (view)
import Control.Lens.Unsound (lensProduct)
import Control.Monad.IO.Unlift (MonadUnliftIO(..), liftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text (Text)
import qualified HsLua.Aeson as LA
import qualified HsLua.Core as Lua
import qualified HsLua.Marshalling as LM
import HsLua.Packaging.Function
  ( DocumentedFunction
  , (<#>), (###), (=#>)
  , defun
  , parameter
  , pushDocumentedFunction
  )
import qualified Network.MQTT.Client as MQTT
import Network.MQTT.Topic (mkTopic)
import qualified Service.App as App
import Service.App (Logger(..), MonadMQTT(..))
import qualified Service.Automation as Automation
import Service.Automation (Automation(..))
import qualified Service.AutomationName as AutomationName
import Service.Env
  ( Env
  , LogLevel(Debug)
  , LoggerVariant(..)
  , MQTTClientVariant(..)
  , config
  , logger
  , luaScriptPath
  , messageQueue
  , mqttClient
  )
import qualified Service.Messages.Daemon as Daemon
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM (TChan, TQueue, atomically, writeTQueue)


luaAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => FilePath
  -> Automation m
luaAutomation filepath =
  Automation
    { _name = AutomationName.LuaScript filepath
    -- these two will need to be dynamically definable and resettable
    -- within this Automation if this is going to make sense for lua
    , _devices = []
    , _wantsFullControlOver = []
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
  (logger', mqttClient') <- view $ lensProduct logger mqttClient
  messageQueue' <- view messageQueue
  luaScriptPath' <- view $ config . luaScriptPath
  luaState <- liftIO Lua.newstate

  luaStatusString <- liftIO . Lua.unsafeRunWith luaState $ do
    Lua.openlibs -- load the default Lua packages
    loadDSL filepath logger' mqttClient' messageQueue'
    (Lua.loadfile $ luaScriptPath' <> filepath) *> Lua.callTrace 0 0
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
  (logger', mqttClient') <- view $ lensProduct logger mqttClient
  messageQueue' <- view messageQueue
  luaScriptPath' <- view $ config . luaScriptPath
  luaState <- liftIO Lua.newstate

  luaStatusString <- liftIO . Lua.unsafeRunWith luaState $ do
    Lua.openlibs -- load the default Lua packages
    loadDSL filepath logger' mqttClient' messageQueue'
    -- apparently you need call/callTrace after loadfile to execute
    -- the chunk (whatever this actually means, it's not clear to
    -- me--is it running the script?), otherwise you can't load a
    -- function up and run it. I wish this was better documented so I
    -- didn't have to bang my head against reference docs without a
    -- clue for an hour before spelunking in the HsLua test code
    -- happened to yield results (see
    -- hslua-core/test/HsLua/CoreTests.hs)
    (Lua.loadfile $ luaScriptPath' <> filepath) *> Lua.callTrace 0 0
    loopAutomation

  debug $ "Lua loopAutomation finished with status '" <> T.pack (show luaStatusString) <> "'."

  where
    -- this is here so we can have an event-loop kinda thing that is
    -- interruptible by AsyncExceptions, vs. doing `while (true) ...`
    -- in Lua which blocks forever.
    loopAutomation :: Lua.LuaE Lua.Exception ()
    loopAutomation = do
      _ <- Lua.getglobal "loopAutomation"
      Lua.callTrace 0 1
      loopAutomation

loadDSL ::
  FilePath -> LoggerVariant -> MQTTClientVariant -> TQueue Daemon.Message -> Lua.LuaE Lua.Exception ()
loadDSL filepath logger' mqttClient' messageQueue' = do
    pushDocumentedFunction publish *> Lua.setglobal "publish"
    pushDocumentedFunction publishJSON *> Lua.setglobal "publishJSON"
    pushDocumentedFunction sendMessage *> Lua.setglobal "sendMessage"
    pushDocumentedFunction logDebugMsg *> Lua.setglobal "logDebugMsg"
    pushDocumentedFunction sleep *> Lua.setglobal "sleep"

  where
    publishImpl :: Text -> ByteString -> Lua.LuaE Lua.Exception ()
    publishImpl topic msg =
      let
        topic' = fromMaybe "" $ mkTopic topic
      in
        -- this is testing-motivated boilerplate
        case mqttClient' of
          MCClient mc -> do
            liftIO $ MQTT.publish mc topic' msg False
          TQClient _textTQ -> pure ()

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
             atomically <$> writeTQueue messageQueue' <$> decode msg
          )
      <#> parameter LM.peekLazyByteString "string" "message" "string to log"
      =#> []

    logDebugMsg :: DocumentedFunction Lua.Exception
    logDebugMsg =
      defun "logDebugMsg"
      ### liftIO . logDebugMsg' filepath logger'
      <#> parameter LM.peekText "string" "logString" "string to log"
      =#> []

    sleep :: DocumentedFunction Lua.Exception
    sleep =
      defun "sleep"
      ### threadDelay . (* 1000000)
      <#> parameter LM.peekIntegral "int" "seconds" "seconds to delay thread"
      =#> []

logDebugMsg' :: FilePath -> LoggerVariant -> Text -> IO ()
logDebugMsg' filepath logger' msg =
  -- this is testing-motivated boilerplate
  case logger' of
    TFLogger tfLogger -> liftIO $
      App.log tfLogger Debug (T.pack filepath <>  " : " <> msg)
    QLogger qLogger -> do
      atomically . writeTQueue qLogger $ T.pack filepath <> ": " <> msg
