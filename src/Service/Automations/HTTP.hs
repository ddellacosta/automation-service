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
import Service.App (Logger (..), debug)
import qualified Service.Automation as Automation
import Service.Automation (Automation (..))
import qualified Service.AutomationName as AutomationName
import Service.Env (Env, devicesRawJSON)
import Service.MQTT.Class (MQTTClient)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM (TChan, TVar, readTVarIO)
import Web.Scotty (file, get, middleware, raw, scottyApp, setHeader)

httpAutomation
  :: (Logger l, MQTTClient mc, MonadReader (Env l mc) m, MonadUnliftIO m)
  => UTCTime
  -> Automation m
httpAutomation ts =
  Automation
    { _name = AutomationName.HTTP
    , _cleanup = mkCleanupAutomation
    , _run = mkRunAutomation
    , _startTime = ts
    }

mkCleanupAutomation
  :: (Logger l, MQTTClient mc, MonadReader (Env l mc) m, MonadUnliftIO m)
  => (TChan Automation.Message -> m ())
mkCleanupAutomation = \_broadcastChan -> do
  debug $ "Starting Cleanup: HTTP"


mkRunAutomation
  :: (Logger l, MQTTClient mc, MonadReader (Env l mc) m, MonadUnliftIO m)
  => (TChan Automation.Message -> m ())
mkRunAutomation = \_broadcastChan -> do
  debug $ "Beginning run of HTTP"

  let
    port = 8080
    settings = Warp.setPort port Warp.defaultSettings

  devices <- view devicesRawJSON
  web' <- liftIO $ web devices

  liftIO $
    Warp.runSettings settings $
      WaiWs.websocketsOr WS.defaultConnectionOptions (ws devices) web'

  where
    web :: TVar (ByteString) -> IO Wai.Application
    web devices = scottyApp $ do
      middleware $ staticPolicy $ addBase "ui"
      get "/" $ file "ui/index.html"
      get "/devices" $ do
        setHeader "Content-Type" "application/json; charset=utf-8"
        raw =<< readTVarIO devices

    ws :: TVar (ByteString) -> WS.ServerApp
    ws devices pending = do
      putStrLn "ws connected"
      conn <- WS.acceptRequest pending
      WS.withPingThread conn 30 (pure ()) $ do
        (msg :: Text) <- WS.receiveData conn
        putStrLn $ show msg
        WS.sendTextData conn =<< readTVarIO devices
        forever $ do
          threadDelay $ 1 * 1000000
