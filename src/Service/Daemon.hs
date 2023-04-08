module Service.Daemon
  ( ThreadMap
  , loadResources -- exported for test use
  , run
  , run' -- exported for test use
  )
where

import Prelude hiding (filter)

import Control.Lens (Lens', (&), (.~), view)
import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO, liftIO)
import Control.Monad.Reader (MonadReader)
import qualified Data.Aeson as Aeson
import Data.Aeson (Value, decode, object)
import Data.Foldable (for_)
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Traversable (for)
import qualified Data.Vector as V
import Network.MQTT.Topic (Topic)
import qualified Service.Automation as Automation
import Service.Automation (Message(..))
import Service.AutomationName (AutomationName(..), parseAutomationNameText, serializeAutomationName)
import Service.Automations (findAutomation)
import Service.App (Logger(..), MonadMQTT(..))
import qualified Service.Device as Device
import Service.Env
  ( AutomationEntry
  , Env
  , RestartConditions(..)
  , ThreadMap
  , appCleanup
  , automationBroadcast
  , config
  , daemonBroadcast
  , dbPath
  , deviceRegistrations
  , devices
  , groupRegistrations
  , groups
  , loadedDevices
  , loadedGroups
  , messageChan
  , mqttConfig
  , mqttDispatch
  , notAlreadyRestarted
  , restartConditions
  , scheduledJobs
  , statusTopic
  , subscriptions
  )
import qualified Service.Group as Group
import qualified Service.MQTT.Messages.Daemon as Daemon
import Service.MQTT.Messages.Daemon (AutomationSchedule)
import Service.MQTT.Status (encodeAutomationStatus)
import qualified Service.StateStore as StateStore
import System.Cron (addJob, execSchedule)
import UnliftIO.Async (Async, async, cancel)
import UnliftIO.Concurrent (killThread)
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

  flip finally (cleanupAutomations appCleanup' threadMapTV) $ do
    debug . T.pack . show $ config'
    --
    -- Considering that we try to load previously running Automations
    -- after a restart, this represents the first and only
    -- initialization of StateManager necessary. Once it's been run
    -- once in a given installation, the following times this is run
    -- it'll probably be shut down immediately after RestartConditions
    -- are met and previously running Automations get loaded. See
    -- tryRestoreRunningAutomations.
    --
    view daemonBroadcast >>= \db ->
      atomically . writeTChan db $ Daemon.Start StateManager
    go

  where
    go = do
      tryRestoreRunningAutomations

      messageChan' <- view messageChan
      daemonBroadcast' <- view daemonBroadcast
      msg <- atomically $ readTChan messageChan'

      debug $ "Daemon received Message: " <> T.pack (show msg)

      case msg of
        Daemon.Start automationName -> do
          initializeAndRunAutomation threadMapTV automationName
          signalStateUpdate threadMapTV
          publishUpdatedStatus threadMapTV
          go

        Daemon.Stop automationName -> do
          stopAutomation threadMapTV automationName
          signalStateUpdate threadMapTV
          publishUpdatedStatus threadMapTV
          go

        Daemon.SendTo automationName msg' ->
          sendClientMsg automationName msg' *> go

        Daemon.Schedule jobId automationSchedule automationMessage -> do
          runScheduledMessage
            jobId automationMessage automationSchedule daemonBroadcast'
          publishUpdatedStatus threadMapTV
          go

        Daemon.Unschedule jobId -> do
          unscheduleJob jobId
          publishUpdatedStatus threadMapTV
          go

        Daemon.DeviceUpdate newDevices -> do
          view devices >>= \stored ->
            loadResources Device._id stored newDevices
          updateRestartConditionsSet loadedDevices True
          go

        Daemon.GroupUpdate newGroups -> do
          view groups >>= \stored ->
            loadResources Group._id stored newGroups
          updateRestartConditionsSet loadedGroups True
          go

        Daemon.RegisterDevice deviceId automationName -> do
          view deviceRegistrations >>= \deviceRegs ->
            addRegisteredResource deviceId automationName deviceRegs
          go

        Daemon.RegisterGroup groupId automationName -> do
          view groupRegistrations >>= \groupRegs ->
            addRegisteredResource groupId automationName groupRegs
          go

        Daemon.Subscribe automationName mTopic listenerBcastChan -> do
          for_ mTopic $ \topic -> do
            subscribe automationName topic listenerBcastChan
          go

        Daemon.Null -> debug "Null Automation" *> go

cleanupAutomations
  :: (Logger m, MonadIO m, MonadReader Env m, MonadUnliftIO m)
  => IO ()
  -> TVar (ThreadMap m)
  -> m ()
cleanupAutomations appCleanup' threadMapTV = do
  threadMap <- atomically . readTVar $ threadMapTV
  for_ (M.toList threadMap) $ \(automationName, (_, async')) -> do
    info $ "Shutting down Automation " <> serializeAutomationName automationName
    cancel async'
  liftIO appCleanup'

tryRestoreRunningAutomations :: (MonadIO m, MonadReader Env m) => m ()
tryRestoreRunningAutomations = do
  rc <- view restartConditions
  -- Possible to get a race condition here? I don't think so, because
  -- the only thing that ever accesses the RestartConditions is this
  -- single Daemon thread. I'd put it all in an atomically block
  -- regardless but for the StateStore call below.
  rc' <- atomically . readTVar $ rc
  case rc' of
    (RestartConditions True True True) -> do
      daemonBroadcast' <- view daemonBroadcast
      dbPath' <- view $ config . dbPath
      storedRunningAutos <- liftIO $ StateStore.allRunning dbPath'
      for_ storedRunningAutos $ \(_id, autoName) ->
        atomically . writeTChan daemonBroadcast' $
          fromMaybe Daemon.Null $ Daemon.Start <$> parseAutomationNameText autoName
      atomically . writeTVar rc $ rc' & notAlreadyRestarted .~ False
    _ -> pure ()

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

    mPriorAutomationAsync <- atomically $
      insertAutomation threadMapTV automationName (automation, clientAsync)

    maybe (pure ()) cancel mPriorAutomationAsync

  where
    --
    -- Given a TVar ThreadMap and a (AutomationName, (Automation m, Async ()))
    -- pair, inserts a new entry into the ThreadMap, or it replaces
    -- the previous entry. If an entry already exists for that
    -- AutomationName, then the previous entry's Async () is returned.
    --
    insertAutomation
      :: TVar (ThreadMap m) -> AutomationName -> AutomationEntry m -> STM (Maybe (Async ()))
    insertAutomation threadMapTV' automationName' automationEntry = do
      threadMap' <- readTVar threadMapTV'
      let mPriorAutomation = M.lookup automationName' threadMap'
      writeTVar threadMapTV' $ M.insert automationName' automationEntry threadMap'
      pure $ snd <$> mPriorAutomation

stopAutomation
  :: (Logger m, MonadReader Env m, MonadUnliftIO m)
  => TVar (ThreadMap m)
  -> AutomationName
  -> m ()
stopAutomation threadMapTV automationName = do
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
  -- I assume the thread blocks when the channel is being read because
  -- UnliftIO.Exception's bracket runs the main thread masked, but
  -- is it just caused by the `calltrace` call blocking in LuaScript,
  -- as that'll create a bound thread?
  --
  for_ (M.lookup automationName threadMap) (async . cancel . snd)

  subscriptions' <- view subscriptions
  atomically $ do
    writeTVar threadMapTV . M.delete automationName $ threadMap

    -- we have to send a final message to any topic channels that the
    -- automation has open so that they won't block when we cancel
    subs <- readTVar $ subscriptions'
    for_ (M.lookup automationName subs) $
      traverse
        (\bc -> writeTChan bc (object [("shutdownMessage", Aeson.Bool True)]))

signalStateUpdate :: (MonadIO m, MonadReader Env m) => TVar (ThreadMap m) -> m ()
signalStateUpdate threadMapTV = do
  runningAutos <- M.keys <$> (atomically . readTVar $ threadMapTV)
  sendClientMsg StateManager $
    Aeson.Array . V.fromList $ Aeson.String . serializeAutomationName <$> runningAutos

publishUpdatedStatus
  :: (MonadIO m, MonadMQTT m, MonadReader Env m) => TVar (ThreadMap m) -> m ()
publishUpdatedStatus threadMapTV = do
  statusTopic' <- view $ config . mqttConfig . statusTopic
  deviceRegs <- view deviceRegistrations
  groupRegs <- view groupRegistrations
  scheduled <- view scheduledJobs
  statusMsg <- atomically $ do
    running <- readTVar threadMapTV
    scheduled' <- readTVar scheduled
    deviceRegs' <- readTVar deviceRegs
    groupRegs' <- readTVar groupRegs
    pure $ encodeAutomationStatus running scheduled' deviceRegs' groupRegs'
  publishMQTT statusTopic' statusMsg

sendClientMsg
  :: (MonadIO m, MonadReader Env m) => AutomationName -> Value -> m ()
sendClientMsg automationName msg = do
  automationBroadcast' <- view automationBroadcast
  atomically . writeTChan automationBroadcast' . Client automationName $ msg

runScheduledMessage
  :: (Logger m, MonadIO m, MonadReader Env m)
  => Daemon.JobId
  -> Daemon.Message
  -> AutomationSchedule
  -> TChan Daemon.Message
  -> m ()
runScheduledMessage jobId automationMessage automationSchedule messageChan' = do
  let dispatchScheduledMessage = atomically . writeTChan messageChan' $ automationMessage

  schedulerThreads <- liftIO . execSchedule $
    addJob dispatchScheduledMessage automationSchedule

  scheduledJobs' <- view scheduledJobs

  case schedulerThreads of
    [threadId] -> do
      sjs <- atomically . readTVar $ scheduledJobs'
      let mPriorThreadId = M.lookup jobId sjs
      atomically $ modifyTVar' scheduledJobs' $
        M.insert jobId (automationSchedule, automationMessage, threadId)
      maybe (pure ()) (\(_, _, priorThreadId) -> killThread priorThreadId) mPriorThreadId

    --
    -- I can't understand why either of the two possibilities below
    -- would happen, but better be safe than sorry:
    --

    [] -> warn "Received no ThreadIds back when running execSchedule."

    ids -> do
      mapM_ killThread ids
      warn
        "Received multiple ThreadIds back when running execSchedule, all have been cancelled."

unscheduleJob :: (MonadIO m, MonadReader Env m) => Daemon.JobId -> m ()
unscheduleJob jobId = do
  scheduledJobs' <- view scheduledJobs
  mPriorThreadId <- atomically $ do
    scheduledJob <- M.lookup jobId <$> readTVar scheduledJobs'
    for scheduledJob $ \(_, _, priorThreadId) -> do
      modifyTVar' scheduledJobs' $ M.delete jobId
      pure priorThreadId
  maybe (pure ()) killThread mPriorThreadId

loadResources
  :: (MonadUnliftIO m, Hashable k) => (v -> k) -> TVar (HashMap k v) -> [v] -> m ()
loadResources mkResourceKey stored newResources =
  atomically . writeTVar stored . M.fromList $
    (\r -> (mkResourceKey r, r)) <$> newResources

updateRestartConditionsSet
  :: (MonadIO m, MonadReader Env m) => Lens' RestartConditions Bool -> Bool -> m ()
updateRestartConditionsSet field conditionState = do
  restartConditions' <- view restartConditions
  atomically $ modifyTVar' restartConditions' $ field .~ conditionState

addRegisteredResource
  :: (MonadReader Env m, MonadUnliftIO m, Hashable k)
  => k
  -> AutomationName
  -> TVar (HashMap k (NonEmpty AutomationName))
  -> m ()
addRegisteredResource resourceId newAutoName resourceStore =
  atomically $ modifyTVar' resourceStore $
    M.insertWith (<>) resourceId $ newAutoName :| []

subscribe
  :: (MonadIO m, MonadMQTT m, MonadReader Env m)
  => AutomationName
  -> Topic
  -> TChan Value
  -> m ()
subscribe automationName topic listenerBcastChan = do
  subscriptions' <- view subscriptions
  mqttDispatch' <- view mqttDispatch
  atomically $ do
    modifyTVar' mqttDispatch' $
      M.insertWith (<>) topic $ mkDefaultTopicMsgAction :| []
    modifyTVar' subscriptions' $
      M.insertWith (<>) automationName $ listenerBcastChan :| []
  subscribeMQTT topic

  where
    mkDefaultTopicMsgAction = \topicMsg ->
      for_ (decode topicMsg) $ atomically . writeTChan listenerBcastChan
