module Service.Automations.HTTP
  ( httpAutomation
  )
where

import Control.Lens (view)
import Control.Monad (forever)
import Control.Monad.IO.Unlift (MonadUnliftIO(..), liftIO)
import Control.Monad.Reader (MonadReader)
import Data.Time.Clock (UTCTime)
import Data.Text (Text)
import Service.App (Logger(..), MonadMQTT(..))
import Service.Env (Env, devices)
import qualified Service.Automation as Automation
import Service.Automation (Automation(..))
import qualified Service.AutomationName as AutomationName
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM (TChan, readTVarIO)
import Web.Scotty (file, get, json, middleware, scottyApp)
import Network.Wai.Middleware.Static (addBase, staticPolicy)

import qualified Network.Wai.Handler.WebSockets as WaiWs
import qualified Network.WebSockets as WS
-- import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

httpAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
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
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => (TChan Automation.Message -> m ())
mkCleanupAutomation = \_broadcastChan -> do
  debug $ "Starting Cleanup: HTTP"


mkRunAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => (TChan Automation.Message -> m ())
mkRunAutomation = \_broadcastChan -> do
  debug $ "Beginning run of HTTP"

  let
    port = 8080
    settings = Warp.setPort port Warp.defaultSettings

  devices' <- view devices
  web' <- liftIO $ web =<< readTVarIO devices'

  liftIO $
    Warp.runSettings settings $ WaiWs.websocketsOr WS.defaultConnectionOptions ws web'

  where
    web devices' = scottyApp $ do
      middleware $ staticPolicy $ addBase "ui"
      get "/" $ file "ui/index.html"
      get "/devices" $ json devices'

    ws :: WS.ServerApp
    ws pending = do
      putStrLn "ws connected"
      conn <- WS.acceptRequest pending
      WS.withPingThread conn 30 (pure ()) $ do
        (msg :: Text) <- WS.receiveData conn
        WS.sendTextData conn $ ("initial> " :: Text) <> msg
        forever $ do
          WS.sendTextData conn $ ("loop data" :: Text)
          threadDelay $ 1 * 1000000
