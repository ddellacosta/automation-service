module Service.Automations.HTTP
  ( httpAutomation
  )
where

import Control.Lens (view)
import Control.Monad (forever)
import Control.Monad.IO.Unlift (MonadUnliftIO (..), liftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (for_)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWs
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import qualified Network.WebSockets as WS
import Service.App (Logger (..), debug)
import qualified Service.App.Logger as Logger
import qualified Service.Automation as Automation
import Service.Automation (Automation (..), ClientMsg (..), Message (..))
import qualified Service.AutomationName as AutomationName
import Service.AutomationName (Port)
import Service.Env (Env, LogLevel (..), daemonBroadcast, devicesRawJSON, groupsRawJSON, logger)
import qualified Service.MQTT.Messages.Daemon as Daemon
import UnliftIO.Async (concurrently_)
import UnliftIO.STM (TChan, TVar, atomically, dupTChan, readTChan, readTVarIO, writeTChan)
import Web.Scotty (file, get, middleware, raw, scottyApp, setHeader)

httpAutomation
  :: (Logger l, MonadReader (Env l mc) m, MonadUnliftIO m)
  => Port
  -> UTCTime
  -> Automation m
httpAutomation port ts =
  Automation
    { _name = AutomationName.HTTP port
    , _cleanup = mkCleanupAutomation
    , _run = mkRunAutomation port
    , _startTime = ts
    }


mkCleanupAutomation
  :: (Logger l, MonadReader (Env l mc) m, MonadUnliftIO m)
  => (TChan Automation.Message -> m ())
mkCleanupAutomation = \_broadcastChan -> do
  debug $ "Starting Cleanup: HTTP"


mkRunAutomation
  :: (Logger l, MonadReader (Env l mc) m, MonadUnliftIO m)
  => Port
  -> TChan Automation.Message
  -> m ()
mkRunAutomation port broadcastChan = do
  debug $ "Beginning run of HTTP"

  devices <- view devicesRawJSON
  groups <- view groupsRawJSON
  web' <- liftIO $ web devices groups

  logger' <- view logger
  daemonBroadcast' <- view daemonBroadcast

  let
    settings = Warp.setPort (fromIntegral port) Warp.defaultSettings

  liftIO $
    Warp.runSettings settings $
      WaiWs.websocketsOr WS.defaultConnectionOptions
        (ws logger' devices groups daemonBroadcast')
        web'

  where
    web :: TVar ByteString -> TVar ByteString -> IO Wai.Application
    web devices groups = scottyApp $ do
      middleware $ staticPolicy $ addBase "ui"

      get "/" $ file "ui/index.html"

      get "/devices" $ do
        setHeader "Content-Type" "application/json; charset=utf-8"
        raw =<< readTVarIO devices

      get "/groups" $ do
        setHeader "Content-Type" "application/json; charset=utf-8"
        raw =<< readTVarIO groups

    ws
      :: (Logger logger)
      => logger
      -> TVar ByteString
      -> TVar ByteString
      -> TChan Daemon.Message
      -> WS.ServerApp
    ws logger' devices groups daemonBC pending  = do
      logDebugMsg logger' "WebSockets connected"
      conn <- WS.acceptRequest pending

      -- initialize device data
      logDebugMsg logger' "Sending Device data to client"
      WS.sendTextData conn =<< readTVarIO devices

      logDebugMsg logger' "Sending Group data to client"
      WS.sendTextData conn =<< readTVarIO groups

      broadcastChanCopy <- atomically . dupTChan $ broadcastChan

      WS.withPingThread conn 30 (pure ()) $ concurrently_
        -- Wait for Daemon.Message values coming back from the
        -- WebSocket connection.
        (forever $ do
            received <- WS.receiveData conn
            logDebugMsg logger' $ "Received via WebSocket: " <> (T.pack $ show received)
            for_ (decode received) $ atomically . writeTChan daemonBC
        )
        -- Coming from the other direction, wait for messages from the
        -- broadcastChan that need to get passed to the WebSocket
        -- connection. At the present time this means only messages
        -- published to MQTT topics subscribed to by the client on the
        -- other end of the WebSocket connection.
        (forever $ do
            msg <- atomically . readTChan $ broadcastChanCopy
            logDebugMsg logger' $ "Received on broadcastChan: " <> (T.pack $ show msg)
            case msg of
              Client (AutomationName.HTTP msgPort) (ValueMsg v)
                | msgPort == port -> WS.sendTextData conn $ encode v
                | otherwise -> pure ()
              _ -> pure ()
        )

    logDebugMsg :: (Logger logger) => logger -> Text -> IO ()
    logDebugMsg logger' msg =
      Logger.log logger' Debug ("HTTP: " <> msg)
