module Service.Automations.HTTP
  ( httpAutomation
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO(..), liftIO)
import Control.Monad.Reader (MonadReader)
import Data.Time.Clock (UTCTime)
import Service.App (Logger(..), MonadMQTT(..))
import Service.Env (Env)
import qualified Service.Automation as Automation
import Service.Automation (Automation(..))
import qualified Service.AutomationName as AutomationName
import UnliftIO.STM (TChan)
import Web.Scotty (file, get, html, middleware, param, scotty)
import Network.Wai.Middleware.Static (addBase, staticPolicy)

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
  liftIO web

  where
    web = scotty 3000 $ do
      middleware $ staticPolicy $ addBase "ui"
      get "/" $ file "ui/index.html"

