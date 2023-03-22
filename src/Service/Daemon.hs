module Service.Daemon
  ( ThreadMap
  , loadResources -- exported for test use
  , run
  , run' -- exported for test use
  )
where

import Prelude hiding (filter)

import Control.Lens (view)
import Control.Monad (void)
import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO, liftIO)
import Control.Monad.Reader (MonadReader)
import qualified Data.Aeson as Aeson
import Data.Aeson (Value, decode, object)
import Data.Foldable (for_)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.Text as T
import qualified Database.SQLite.Simple as DB
import qualified Service.Automation as Automation
import Service.Automation (Automation, Message(..))
import Service.AutomationName (AutomationName(..), serializeAutomationName)
import Service.Automations (findAutomation)
import Service.App (Logger(..), MonadMQTT(..))
import qualified Service.Device as Device
import Service.Env
  ( Env
  , Subscriptions
  , appCleanup
  , automationBroadcast
  , config
  , dbPath
  , deviceRegistrations
  , devices
  , groupRegistrations
  , groups
  , messageChan
  , mqttDispatch
  , serverChan
  , subscriptions
  )
import qualified Service.Group as Group
import qualified Service.Messages.Daemon as Daemon
import Service.Messages.Daemon (AutomationSchedule)
import System.Cron (addJob, execSchedule)
import UnliftIO.Async (Async, async, cancel)
import UnliftIO.Exception (bracket, finally)
import UnliftIO.STM
  ( STM
  , TChan
  , TVar
  , atomically
  , dupTChan
  , modifyTVar'
  , newTVarIO
  , readTChan
  , readTVar
  , writeTChan
  , writeTVar
  )

type AutomationEntry m = (Automation m, Async ())

type ThreadMap m = M.Map AutomationName (AutomationEntry m)

run
  :: (Logger m, MonadReader Env m, MonadMQTT m, MonadUnliftIO m)
  => m ()
run = do
  threadMapTV <- newTVarIO M.empty
  run' threadMapTV

--
-- Splitting this from the above is a bit of a hack to enable easily
-- testing threadMapTV without needing to include it in Env, which
-- brings the Monad m type variable into play, which makes it a pain
-- to deal with. Anyways ThreadMap is really only a concern of this
-- module anyways, so it's probably for the best regardless.
--
run'
  :: (Logger m, MonadReader Env m, MonadMQTT m, MonadUnliftIO m)
  => TVar (ThreadMap m)
  -> m ()
run' threadMapTV = do
  config' <- view config
  appCleanup' <- view appCleanup

  flip finally (cleanupAutomations appCleanup' threadMapTV)
    (debug . T.pack . show $ config') *> go

  where
    go = do
      messageChan' <- view messageChan
      msg <- atomically $ readTChan messageChan'

      debug $ "Daemon received Message: " <> T.pack (show msg)

      case msg of
        Daemon.Start automationName -> do
          dbPath' <- view $ config . dbPath
          liftIO $ do
            dbConn <- DB.open dbPath'
            -- this should be configurable somehow, and should dump to debug log entries
            -- DB.setTrace dbConn $ Just $ \t -> print t
            DB.execute_ dbConn
              "CREATE TABLE IF NOT EXISTS running (id INTEGER PRIMARY KEY, automationName TEXT) STRICT"
            DB.execute
              dbConn
              "INSERT INTO running (automationName) VALUES (?)"
              (DB.Only (serializeAutomationName automationName))
          initializeAndRunAutomation threadMapTV automationName
          go

        Daemon.Stop automationName -> do
          dbPath' <- view $ config . dbPath
          subscriptions' <- view subscriptions
          liftIO $ do
            dbConn <- DB.open dbPath'
            -- this should be configurable somehow, and should dump to debug log entries
            -- DB.setTrace dbConn $ Just $ \t -> print t
            DB.execute_ dbConn
              "CREATE TABLE IF NOT EXISTS running (id INTEGER PRIMARY KEY, automationName TEXT) STRICT"
            DB.execute
              dbConn
              "DELETE FROM running WHERE automationName = ?"
              ([serializeAutomationName automationName] :: [T.Text])
          stopAutomation threadMapTV subscriptions' automationName *> go
          go

        Daemon.SendTo automationName msg' -> do
          serverChan' <- view serverChan
          sendClientMsg automationName serverChan' msg' *> go

        Daemon.Schedule automationMessage automationSchedule ->
          addScheduleAutomationMessage
            automationMessage automationSchedule messageChan' *> go

        Daemon.DeviceUpdate devices' -> do
          storedDevices <- view devices
          loadResources Device._id storedDevices devices' *> go

        Daemon.GroupUpdate groups' -> do
          storedGroups <- view groups
          loadResources Group._id storedGroups groups' *> go

        Daemon.RegisterDevice deviceId automationName -> do
          deviceRegs <- view deviceRegistrations
          addRegisteredResource deviceId automationName deviceRegs *> go

        Daemon.RegisterGroup groupId automationName -> do
          groupRegs <- view groupRegistrations
          addRegisteredResource groupId automationName groupRegs *> go

        Daemon.Subscribe automationName mTopic listenerBcastChan -> do
          mqttDispatch' <- view mqttDispatch
          subscriptions' <- view subscriptions
          for_ mTopic $ \topic -> do
            atomically $ do
              modifyTVar' mqttDispatch' $
                M.insertWith (<>) topic $
                  mkDefaultTopicMsgAction listenerBcastChan :| []
              modifyTVar' subscriptions' $
                M.insertWith (<>) automationName $ listenerBcastChan :| []
            subscribeMQTT topic
          *> go

        Daemon.Null -> debug "Null Automation" *> go

    mkDefaultTopicMsgAction listenerBcastChan = \topicMsg ->
      for_ (decode topicMsg) $ atomically . writeTChan listenerBcastChan

initializeAndRunAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => TVar (ThreadMap m)
  -> AutomationName
  -> m ()
initializeAndRunAutomation
  threadMapTV automationName = do
    automationBroadcast' <- view automationBroadcast
    clientChan <- atomically $ dupTChan automationBroadcast'

    let automation = findAutomation automationName

    --
    -- > The more subtle difference is that this function will use
    -- > uninterruptible masking for its cleanup handler. This is a
    -- > subtle distinction, but at a high level, means that resource
    -- > cleanup has more guarantees to complete. This comes at the cost
    -- > that an incorrectly written cleanup function cannot be
    -- > interrupted.
    --
    -- https://hackage.haskell.org/package/unliftio-0.2.24.0/docs/UnliftIO-Exception.html#v:bracket
    --
    clientAsync <- async $
      bracket (pure clientChan) (Automation._cleanup automation) (Automation._run automation)

    atomically $
      insertAutomation threadMapTV automationName (automation, clientAsync)

  where
    --
    -- Given a TVar ThreadMap and a (AutomationName, (Automation m, Async ()))
    -- pair, inserts a new entry into the ThreadMap. If the ThreadMap
    -- already contains a List of (Automation m, Async ()) pairs at that
    -- index it will append a new one to the end of that List, otherwise
    -- it will add a new List with the new entry as its first member.
    --
    insertAutomation :: TVar (ThreadMap m) -> AutomationName -> AutomationEntry m -> STM ()
    insertAutomation threadMapTV' automationName' automationEntry = do
      threadMap' <- readTVar threadMapTV'
      writeTVar threadMapTV' $ M.insert automationName' automationEntry threadMap'

stopAutomation
  :: (Logger m, MonadUnliftIO m)
  => TVar (ThreadMap m)
  -> TVar Subscriptions
  -> AutomationName
  -> m ()
stopAutomation threadMapTV subscriptions' automationName = do
  info $ "Shutting down Automation " <> serializeAutomationName automationName

  threadMap <- atomically . readTVar $ threadMapTV

  --
  -- The cancel below is somewhat ironically wrapped in an async
  -- because cancel will block when a topic channel is being read from
  -- inside an automation thread and we need to send a message to the
  -- topic channel immediately after, so we don't want it to
  -- block...especially because the purpose of the message sent to the
  -- topic channel is explicitly to allow the readTChan call inside of
  -- `subscribe` calls to be unblocked so `cancel`'s `AsyncCancelled`
  -- exception is picked up by the thread and shuts it down.
  --
  -- Wrt the topic channel being read from and blocking, I'm not sure
  -- why this is because as I understand it readTChan
  -- (a.k.a. readTVar) should be interruptible, meaning it should
  -- respond to `AsyncCancelled` getting thrown even if it is masked,
  -- so I figure this has more to do with Lua's semantics? and is
  -- being caused somehow by the `calltrace` call blocking in
  -- LuaScript...but this is just a guess.
  --
  for_ (M.lookup automationName threadMap) $ \(_, async') -> async $ cancel async'
  atomically . writeTVar threadMapTV . M.delete automationName $ threadMap

  -- we have to send a final message to any topic channels that the
  -- automation has open so that they won't block when we cancel
  subs <- atomically . readTVar $ subscriptions'
  for_ (M.lookup automationName subs) $
    traverse
      (\bc -> atomically $ writeTChan bc (object [("shutdownMessage", Aeson.Bool True)]))


cleanupAutomations
  :: (Logger m, MonadIO m, MonadReader Env m, MonadUnliftIO m)
  => IO ()
  -> TVar (ThreadMap m)
  -> m ()
cleanupAutomations appCleanup' threadMapTV = do
  threadMap <- atomically . readTVar $ threadMapTV
  for_ (M.assocs threadMap) $ \(automationName, (_, async')) -> do
    info $ "Shutting down Automation " <> serializeAutomationName automationName
    cancel async'
  liftIO appCleanup'

sendClientMsg
  :: (MonadUnliftIO m) => AutomationName -> TChan Message -> Value -> m ()
sendClientMsg automationName serverChan' =
  atomically . writeTChan serverChan' . Client automationName

addScheduleAutomationMessage
  :: (MonadIO m)
  => Daemon.Message
  -> AutomationSchedule
  -> TChan Daemon.Message
  -> m ()
addScheduleAutomationMessage automationMessage automationSchedule messageChan' = do
  void . liftIO . execSchedule $ flip addJob automationSchedule $
    atomically . writeTChan messageChan' $ automationMessage

loadResources
  :: (MonadUnliftIO m, Ord a) => (b -> a) -> TVar (Map a b) -> [b] -> m ()
loadResources mkResourceKey stored newResources =
  atomically . writeTVar stored . M.fromList $
    (\r -> (mkResourceKey r, r)) <$> newResources

addRegisteredResource
  :: (MonadReader Env m, MonadUnliftIO m, Ord k)
  => k
  -> AutomationName
  -> TVar (Map k (NonEmpty AutomationName))
  -> m ()
addRegisteredResource resourceId newAutoName resourceStore = do
  atomically $ modifyTVar' resourceStore $ \resourceStore' ->
    M.alter
      (\case
          Just autos -> Just (NE.append autos $ newAutoName :| [])
          Nothing -> Just (newAutoName :| [])
      )
      resourceId
      resourceStore'
