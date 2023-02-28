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
import HsLua.Core (dofileTrace, openlibs, setglobal)
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
import Service.Automation (Automation(..), Message(..))
import qualified Service.AutomationName as AutomationName
import Service.Env
  ( Env
  , LogLevel(Debug)
  , LoggerVariant(TFLogger)
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
luaAutomation filePath =
  Automation
    { _name = AutomationName.LuaScript filePath
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

  luaStatusString <- liftIO $ Lua.run $ do
    openlibs -- load the default Lua packages
    pushDocumentedFunction (publish mqttClient') *> setglobal "publish"
    pushDocumentedFunction (publishJSON mqttClient') *> setglobal "publishJSON"
    pushDocumentedFunction (sendMessage messageQueue') *> setglobal "sendMessage"
    pushDocumentedFunction (logDebugMsg logger') *> setglobal "logDebugMsg"
    pushDocumentedFunction sleep *> setglobal "sleep"
    status <- dofileTrace $ luaScriptPath' <> filePath
    if (status /= Lua.OK) then
      show <$> Lua.popException
    else
      pure . show $ status

  debug $ "Lua finished with status '" <> T.pack luaStatusString <> "'."

  where
    publishImpl :: MQTTClientVariant -> Text -> ByteString -> Lua.LuaE Lua.Exception ()
    publishImpl mqttClient' topic msg =
      let
        topic' = fromMaybe "" $ mkTopic topic
      in
        -- this is testing-motivated boilerplate
        case mqttClient' of
          MCClient mc -> do
            liftIO $ MQTT.publish mc topic' msg False
          TQClient _textTQ -> pure ()

    publish :: MQTTClientVariant -> DocumentedFunction Lua.Exception
    publish mqttClient' =
      defun "publish"
      ### publishImpl mqttClient'
      <#> parameter LM.peekText "string" "topic" "topic for device"
      <#> parameter LM.peekLazyByteString "string" "msg" "MQTT JSON string msg to send"
      =#> []

    publishJSON :: MQTTClientVariant -> DocumentedFunction Lua.Exception
    publishJSON mqttClient' =
      defun "publish"
      ### (\topic -> publishImpl mqttClient' topic . encode)
      <#> parameter LM.peekText "string" "topic" "topic for device"
      <#> parameter LA.peekValue "table" "jsonMsg" "MQTT JSON string msg to send"
      =#> []

    sendMessage :: TQueue Daemon.Message -> DocumentedFunction Lua.Exception
    sendMessage messageQueue' =
      defun "publish"
      ### (\msg -> fromMaybe (pure ()) $
             atomically <$> writeTQueue messageQueue' <$> decode msg
          )
      <#> parameter LM.peekLazyByteString "string" "message" "string to log"
      =#> []

    logDebugMsg :: LoggerVariant -> DocumentedFunction Lua.Exception
    logDebugMsg logger' =
      defun "logDebugMsg"
      ### (\msg -> case logger' of
             -- this is testing-motivated boilerplate
             TFLogger tfLogger -> liftIO $ App.log tfLogger Debug msg
             _ -> pure ()
          )
      <#> parameter LM.peekText "string" "logString" "string to log"
      =#> []

    sleep :: DocumentedFunction Lua.Exception
    sleep =
      defun "sleep"
      ### threadDelay . (* 1000000)
      <#> parameter LM.peekIntegral "int" "seconds" "seconds to delay thread"
      =#> []
