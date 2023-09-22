module Service.Automations.HTTP
  ( httpAutomation
  )
where

import Control.Lens (view)
import Control.Monad (forever)
import Control.Monad.IO.Unlift (MonadUnliftIO (..), liftIO)
import Control.Monad.Reader (MonadReader)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWs
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import qualified Network.WebSockets as WS
import Service.App (Logger (..), MonadMQTT (..), logWithVariant)
import qualified Service.Automation as Automation
import Service.Automation (Automation (..))
import qualified Service.AutomationName as AutomationName
import Service.AutomationName (Port)
import Service.Env (Env, LogLevel (..), LoggerVariant, devicesRawJSON, logger)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM (TChan, TVar, readTVarIO)
import Web.Scotty (file, get, middleware, raw, scottyApp, setHeader)

httpAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
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
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => (TChan Automation.Message -> m ())
mkCleanupAutomation = \_broadcastChan -> do
  debug $ "Starting Cleanup: HTTP"


mkRunAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => Port
  -> (TChan Automation.Message -> m ())
mkRunAutomation port = \_broadcastChan -> do
  debug $ "Beginning run of HTTP"

  devices <- view devicesRawJSON
  web' <- liftIO $ web devices

  logger' <- view logger

  let
    settings = Warp.setPort (fromIntegral port) Warp.defaultSettings

  liftIO $
    Warp.runSettings settings $
      WaiWs.websocketsOr WS.defaultConnectionOptions (ws logger' devices) web'

  where
    web :: TVar (ByteString) -> IO Wai.Application
    web devices = scottyApp $ do
      middleware $ staticPolicy $ addBase "ui"
      get "/" $ file "ui/index.html"
      get "/devices" $ do
        setHeader "Content-Type" "application/json; charset=utf-8"
        raw =<< readTVarIO devices

    ws :: LoggerVariant -> TVar (ByteString) -> WS.ServerApp
    ws logger' devices pending = do
      putStrLn "ws connected"
      conn <- WS.acceptRequest pending
      WS.withPingThread conn 30 (pure ()) $ do
        (msg :: Text) <- WS.receiveData conn
        logDebugMsg logger' $ "Received: " <> msg
        WS.sendTextData conn =<< readTVarIO devices
        forever $ do
          threadDelay $ 1 * 1000000

logDebugMsg :: LoggerVariant -> Text -> IO ()
logDebugMsg logger' msg =
  logWithVariant logger' Debug ("HTTP: " <> msg)
