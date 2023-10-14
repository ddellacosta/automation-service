module Service.Daemon
  ( ThreadMap
  , loadResources -- exported for test use
  , run
  , run' -- exported for test use
  )
where

import Prelude hiding (filter)

import Control.Lens (Lens', view, (&), (.~), (<&>), (^.))
import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (Value, decode, encode, object)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as SBS
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (foldMap', foldl', for_)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Traversable (for)
import qualified Data.Vector as V
import GHC.Conc (ThreadStatus (..), threadStatus)
import Network.MQTT.Topic (Topic)
import qualified Service.App as App
import Service.App (Logger (..), debug, info, warn)
import qualified Service.Automation as Automation
import Service.Automation (Automation (_name), ClientMsg (..), Message (..))
import Service.AutomationName (AutomationName (..), parseAutomationNameText,
                               serializeAutomationName)
import Service.Automations (findAutomation)
import qualified Service.Device as Device
import Service.Env (AutomationEntry, Env, Registrations, RestartConditions (..), ThreadMap,
                    appCleanup, automationBroadcast, automationServiceTopic, config,
                    daemonBroadcast, dbPath, deviceRegistrations, devices, devicesRawJSON,
                    groupRegistrations, groups, groupsRawJSON, loadedDevices, loadedGroups,
                    messageChan, mqttConfig, mqttDispatch, notAlreadyRestarted, restartConditions,
                    scheduledJobs, startupMessages, subscriptions)
import qualified Service.Group as Group
import Service.MQTT.Class (MQTTClient)
import qualified Service.MQTT.Messages.Daemon as Daemon
import Service.MQTT.Messages.Daemon (AutomationSchedule)
import Service.MQTT.Status (encodeAutomationStatus)
import qualified Service.StateStore as StateStore
import System.Cron (addJob, execSchedule)
import UnliftIO.Async (Async, async, asyncThreadId, cancel)
import UnliftIO.Concurrent (killThread)
import UnliftIO.Exception (bracket, finally)
import UnliftIO.STM (STM, TChan, TVar, atomically, dupTChan, modifyTVar', newTVarIO, readTChan,
                     readTVar, readTVarIO, writeTChan, writeTVar)

run
  :: (Logger l, MQTTClient mc, MonadReader (Env l mc) m, MonadUnliftIO m)
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
  :: (Logger l, MQTTClient mc, MonadReader (Env l mc) m, MonadUnliftIO m)
  => TVar (ThreadMap m)
  -> m ()
run' threadMapTV = do
  config' <- view config
  appCleanup' <- view appCleanup

  priorRunningAutomations <- loadPriorRunningAutomations (config' ^. dbPath)
  priorScheduledAutomations <- loadPriorScheduledAutomations (config' ^. dbPath)

  startupMessages' <- view startupMessages
  atomically . writeTVar startupMessages' $
    priorRunningAutomations <> priorScheduledAutomations

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
    view daemonBroadcast >>= \db -> atomically $ do
      writeTChan db $ Daemon.Start StateManager
      writeTChan db $ Daemon.Start HTTPDefault
    go

  where
    go = do
      tryRestoreRunningAutomations

      messageChan' <- view messageChan
      msg <- atomically $ readTChan messageChan'

      debug $ "Daemon received Message: " <> T.pack (show msg)

      case msg of
        Daemon.Start automationName -> do
          initializeAndRunAutomation threadMapTV automationName
          signalRunningStateUpdate threadMapTV
          publishUpdatedStatus threadMapTV
          go

        Daemon.Stop automationName -> do
          stopAutomation threadMapTV automationName
          signalRunningStateUpdate threadMapTV
          publishUpdatedStatus threadMapTV
          go

        Daemon.SendTo automationName msg' ->
          sendClientMsg automationName msg' *> go

        Daemon.Schedule jobId automationSchedule automationMessage -> do
          runScheduledMessage jobId automationMessage automationSchedule
          signalScheduledStateUpdate
          publishUpdatedStatus threadMapTV
          go

        Daemon.Unschedule jobId -> do
          unscheduleJob jobId
          signalScheduledStateUpdate
          publishUpdatedStatus threadMapTV
          go

        Daemon.DeviceUpdate newDevices devicesRawJSON' -> do
          view devices >>= \stored ->
            loadResources Device._id stored newDevices
          view devicesRawJSON >>= \stored ->
            atomically $ writeTVar stored devicesRawJSON'
          updateRestartConditionsSet loadedDevices True
          go

        Daemon.GroupUpdate newGroups groupsRawJSON' -> do
          view groups >>= \stored ->
            loadResources Group._id stored newGroups
          view groupsRawJSON >>= \stored ->
            atomically $ writeTVar stored groupsRawJSON'
          updateRestartConditionsSet loadedGroups True
          go

        Daemon.RegisterDevice deviceId automationName -> do
          view deviceRegistrations >>= \deviceRegs ->
            addRegisteredResource deviceId automationName deviceRegs
          publishUpdatedStatus threadMapTV
          go

        Daemon.RegisterGroup groupId automationName -> do
          view groupRegistrations >>= \groupRegs ->
            addRegisteredResource groupId automationName groupRegs
          publishUpdatedStatus threadMapTV
          go

        Daemon.DeRegisterDevicesAndGroups automationName -> do
          deregisterDevicesAndGroups automationName
          publishUpdatedStatus threadMapTV
          go

        Daemon.Subscribe automationName mTopic listenerBcastChan -> do
          for_ mTopic $ \topic -> do
            subscribe automationName topic listenerBcastChan
          go

        Daemon.Publish (Daemon.MQTTMsg topic msg') ->
          publishMQTT topic (encode msg') *> go

        Daemon.DeadAutoCleanup -> do
          cleanDeadAutomations threadMapTV
          publishUpdatedStatus threadMapTV
          go

        Daemon.Status -> publishUpdatedStatus threadMapTV *> go

        Daemon.Null -> debug "Null Automation" *> go

    loadPriorRunningAutomations :: (MonadIO m) => FilePath -> m [Daemon.Message]
    loadPriorRunningAutomations dbPath' = liftIO $ do
      allRunning <- StateStore.allRunning dbPath'
      pure . catMaybes $ allRunning <&> \(_, autoNameStr) ->
        Daemon.Start <$> parseAutomationNameText autoNameStr

    loadPriorScheduledAutomations :: (MonadIO m) => FilePath -> m [Daemon.Message]
    loadPriorScheduledAutomations dbPath' = liftIO $ StateStore.allScheduled dbPath' <&>
      fromMaybe [] . sequence . fmap (decode . LBS.fromStrict . snd)

    cleanupAutomations
      :: (MonadIO m, Logger l, MQTTClient mc, MonadReader (Env l mc) m, MonadUnliftIO m)
      => IO ()
      -> TVar (ThreadMap m)
      -> m ()
    cleanupAutomations appCleanup' threadMapTV' = do
      threadMap <- atomically . readTVar $ threadMapTV'
      for_ (M.toList threadMap) $ \(automationName, (_, async')) -> do
        info $ "Shutting down Automation " <> serializeAutomationName automationName
        cancel async'
      liftIO appCleanup'

    cleanDeadAutomations
      :: (MonadIO m, Logger l, MQTTClient mc, MonadReader (Env l mc) m) => TVar (ThreadMap m) -> m ()
    cleanDeadAutomations threadMapTV' = do
      threadMap <- atomically $ readTVar threadMapTV'
      cleanupAutoNames <- liftIO $ foldMap'
        (\(auto, autoAsync) -> do
            autoStatus <- liftIO . threadStatus . asyncThreadId $ autoAsync
            case autoStatus of
              ThreadFinished -> pure [_name auto]
              ThreadDied     -> pure [_name auto]
              _              -> pure []
        )
        threadMap
      let cleanedTM = foldl' (flip M.delete) threadMap cleanupAutoNames
      atomically $ writeTVar threadMapTV' cleanedTM

    tryRestoreRunningAutomations :: (MonadIO m, Logger l, MQTTClient mc, MonadReader (Env l mc) m) => m ()
    tryRestoreRunningAutomations = do
      rc <- view restartConditions
      rc' <- atomically . readTVar $ rc
      case rc' of
        (RestartConditions True True True) -> do
          daemonBroadcast' <- view daemonBroadcast
          priorRunning <- view startupMessages
          atomically $ do
            priorRunning' <- readTVar priorRunning
            for_ priorRunning' $ writeTChan daemonBroadcast'
            writeTVar rc $ rc' & notAlreadyRestarted .~ False
        _ -> pure ()

    initializeAndRunAutomation
      :: (Logger l, MQTTClient mc, MonadReader (Env l mc) m, MonadUnliftIO m)
      => TVar (ThreadMap m)
      -> AutomationName
      -> m ()
    initializeAndRunAutomation
      threadMapTV' automationName = do
        startTime <- liftIO getCurrentTime
        let automation = findAutomation automationName $ startTime

        automationBroadcast' <- view automationBroadcast

        --
        -- > The more subtle difference is that this function will use
        -- > uninterruptible masking for its cleanup handler.
        --
        -- https://hackage.haskell.org/package/unliftio-0.2.24.0/docs/UnliftIO-Exception.html#v:bracket
        --
        clientAsync <- async $
          bracket
            (atomically $ dupTChan automationBroadcast')
            (Automation._cleanup automation)
            (Automation._run automation)

        mPriorAutomationAsync <- atomically $
          insertAutomation threadMapTV' automationName (automation, clientAsync)

        for_ mPriorAutomationAsync cancel

      where
        --
        -- Given a TVar ThreadMap and a (AutomationName, (Automation m, Async ()))
        -- pair, inserts a new entry into the ThreadMap, or it replaces
        -- the previous entry. If an entry already exists for that
        -- AutomationName, then the previous entry's Async () is returned.
        --
        insertAutomation
          :: TVar (ThreadMap m) -> AutomationName -> AutomationEntry m -> STM (Maybe (Async ()))
        insertAutomation threadMapTV'' automationName' automationEntry = do
          threadMap' <- readTVar threadMapTV''
          let mPriorAutomation = M.lookup automationName' threadMap'
          writeTVar threadMapTV'' $ M.insert automationName' automationEntry threadMap'
          pure $ snd <$> mPriorAutomation

    stopAutomation
      :: (Logger l, MQTTClient mc, MonadReader (Env l mc) m, MonadUnliftIO m)
      => TVar (ThreadMap m)
      -> AutomationName
      -> m ()
    stopAutomation threadMapTV' automationName = do
      info $ "Shutting down Automation " <> serializeAutomationName automationName

      threadMap <- atomically . readTVar $ threadMapTV'

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
        writeTVar threadMapTV' . M.delete automationName $ threadMap

        -- we have to send a final message to any topic channels that the
        -- automation has open so that they won't block when we cancel
        subs <- readTVar $ subscriptions'
        for_ (M.lookup automationName subs) $
          traverse
            (\bc -> writeTChan bc (object [("shutdownMessage", Aeson.Bool True)]))

    signalRunningStateUpdate :: (MonadIO m, Logger l, MQTTClient mc, MonadReader (Env l mc) m) => TVar (ThreadMap m) -> m ()
    signalRunningStateUpdate threadMapTV' = do
      runningAutos <- M.keys <$> (atomically . readTVar $ threadMapTV')
      sendClientMsg StateManager $ ValueMsg $
        Aeson.Array . V.fromList $ Aeson.String . serializeAutomationName <$> runningAutos

    publishUpdatedStatus
      :: (MonadIO m, Logger l, MQTTClient mc, MonadReader (Env l mc) m) => TVar (ThreadMap m) -> m ()
    publishUpdatedStatus threadMapTV' = do
      automationServiceTopic' <- view $ config . mqttConfig . automationServiceTopic
      deviceRegs <- view deviceRegistrations
      groupRegs <- view groupRegistrations
      scheduled <- view scheduledJobs
      statusMsg <- atomically $ do
        running <- readTVar threadMapTV'
        scheduled' <- readTVar scheduled
        deviceRegs' <- readTVar deviceRegs
        groupRegs' <- readTVar groupRegs
        pure $ encodeAutomationStatus running scheduled' deviceRegs' groupRegs'
      App.publish automationServiceTopic' statusMsg

    sendClientMsg
      :: (MonadIO m, Logger l, MQTTClient mc, MonadReader (Env l mc) m) => AutomationName -> ClientMsg -> m ()
    sendClientMsg automationName msg = do
      automationBroadcast' <- view automationBroadcast
      atomically . writeTChan automationBroadcast' $
        Client automationName msg

    runScheduledMessage
      :: (MonadIO m, Logger l, MQTTClient mc, MonadReader (Env l mc) m)
      => Daemon.JobId
      -> Daemon.Message
      -> AutomationSchedule
      -> m ()
    runScheduledMessage jobId automationMessage automationSchedule = do
      daemonBroadcast' <- view daemonBroadcast
      let
        dispatchScheduledMessage =
          atomically . writeTChan daemonBroadcast' $ automationMessage

      schedulerThreads <- liftIO . execSchedule $
        addJob dispatchScheduledMessage automationSchedule

      scheduledJobs' <- view scheduledJobs

      case schedulerThreads of
        [threadId] -> do
          -- pull out any existing jobs matching the jobId
          sjs <- atomically . readTVar $ scheduledJobs'
          let mPriorThreadId = M.lookup jobId sjs

          atomically $ do
            sjs' <- readTVar scheduledJobs'
            let
              updatedSjs = M.insert jobId (automationSchedule, automationMessage, threadId) sjs'
            writeTVar scheduledJobs' updatedSjs

          -- if scheduled job already existed, kill the previously running thread
          -- since the new job is started before the old one is
          -- killed, this could cause a race condition if there are
          -- resources both jobs use...hmm
          for_ mPriorThreadId $ \(_, _, priorThreadId) ->
            killThread priorThreadId

        -- these two will probably never happen

        [] -> warn "Received no ThreadIds back when running execSchedule."

        ids -> do
          mapM_ killThread ids
          warn
            "Received multiple ThreadIds back when running execSchedule, all have been cancelled."

    signalScheduledStateUpdate :: (MonadIO m, Logger l, MQTTClient mc, MonadReader (Env l mc) m) => m ()
    signalScheduledStateUpdate = do
      scheduledJobs' <- readTVarIO =<< view scheduledJobs
      sendClientMsg StateManager $ ByteStringMsg $
        flip M.foldMapWithKey scheduledJobs' $ \jobId' (sched, msg, _threadId) ->
          [SBS.concat . LBS.toChunks . encode $ Daemon.Schedule jobId' sched msg]

    unscheduleJob :: (MonadIO m, Logger l, MQTTClient mc, MonadReader (Env l mc) m) => Daemon.JobId -> m ()
    unscheduleJob jobId = do
      scheduledJobs' <- view scheduledJobs
      mPriorThreadId <- atomically $ do
        scheduledJob <- M.lookup jobId <$> readTVar scheduledJobs'
        for scheduledJob $ \(_, _, priorThreadId) -> do
          modifyTVar' scheduledJobs' $ M.delete jobId
          pure priorThreadId
      for_ mPriorThreadId killThread

    updateRestartConditionsSet
      :: (MonadIO m, Logger l, MQTTClient mc, MonadReader (Env l mc) m) => Lens' RestartConditions Bool -> Bool -> m ()
    updateRestartConditionsSet field conditionState = do
      restartConditions' <- view restartConditions
      atomically $ modifyTVar' restartConditions' $ field .~ conditionState

    addRegisteredResource
      :: (Logger l, MQTTClient mc, MonadReader (Env l mc) m, MonadUnliftIO m, Hashable k)
      => k
      -> AutomationName
      -> TVar (HashMap k (NonEmpty AutomationName))
      -> m ()
    addRegisteredResource resourceId newAutoName resourceStore =
      atomically $ modifyTVar' resourceStore $
        M.insertWith (<>) resourceId $ newAutoName :| []

    deregisterDevicesAndGroups
      :: (MonadIO m, Logger l, MQTTClient mc, MonadReader (Env l mc) m) => AutomationName -> m ()
    deregisterDevicesAndGroups automationName = do
      deviceRegs <- view deviceRegistrations
      groupRegs <- view groupRegistrations

      atomically $ do
        updateRegs deviceRegs
        updateRegs groupRegs

      where
        updateRegs :: (Hashable a) => TVar (Registrations a) -> STM ()
        updateRegs regs = modifyTVar' regs $ \regs' ->
          M.foldrWithKey'
            (\idx autos newRegs ->
                case NE.nonEmpty . NE.filter (/= automationName) $ autos of
                  Just autos' -> M.insert idx autos' newRegs
                  Nothing     -> newRegs
            )
            M.empty
            regs'

    subscribe
      :: (MonadIO m, Logger l, MQTTClient mc, MonadReader (Env l mc) m)
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
      App.subscribe topic

      where
        mkDefaultTopicMsgAction = \topicMsg ->
          for_ (decode topicMsg) $ atomically . writeTChan listenerBcastChan

loadResources
  :: (MonadUnliftIO m, Hashable k) => (v -> k) -> TVar (HashMap k v) -> [v] -> m ()
loadResources mkResourceKey stored newResources =
  atomically . writeTVar stored . M.fromList $
    (\r -> (mkResourceKey r, r)) <$> newResources
