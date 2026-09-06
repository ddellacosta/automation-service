module Test.Spec.Reporter.Allure
  ( Attachment
  , addPendingAttachment
  , allureReporter
  , allureResultsDir
  , prepareResultsDir
  ) where

import Data.Argonaut.Core (Json, jsonEmptyObject, jsonNull, stringify)
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.DateTime.Instant (unInstant)
import Data.Foldable (intercalate)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (round, toNumber) as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception as Error
import Effect.Now (now)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Pipes (await, yield)
import Prelude (($), (<>), (+), Unit, bind, discard, pure, show)
import Test.Spec.Result (Result(..))
import Test.Spec.Runner (Reporter)
import Test.Spec.Runner.Event as Event
import Test.Spec.Tree (Path, parentSuiteName)

-- The results directory name; kept here as the single source of truth so
-- the runner and any attachment producers agree on it.
allureResultsDir :: String
allureResultsDir = "allure-results"

-- | An extra file to embed in a test result's Allure entry (e.g. a
-- | screenshot). The contents arrive base64-encoded and are written into
-- | the results directory when the test's result JSON is emitted.
type Attachment =
  { name :: String
  , contentType :: String
  , fileExtension :: String
  , base64Contents :: String
  }

-- Module-level sink: attachments registered while a test runs (e.g. a
-- failure screenshot, taken before the failure propagates to the runner)
-- are consumed by the next result the reporter writes. A module-level
-- Ref is safe here: tests run serially in a single node process.
pendingAttachments :: Ref.Ref (Array Attachment)
pendingAttachments = unsafePerformEffect (Ref.new [])

-- | Register an attachment to be embedded in the next test result.
addPendingAttachment :: Attachment -> Effect Unit
addPendingAttachment att = Ref.modify_ (\atts -> atts <> [ att ]) pendingAttachments

-- FFI: write a UTF-8 string to a file, creating parent dirs as needed
foreign import writeFileSync_ :: String -> String -> Effect Unit

-- FFI: delete the directory if it exists, then recreate it, so each run
-- starts with an empty results dir (stale results would otherwise mix
-- into the generated report)
foreign import prepareResultsDir :: String -> Effect Unit

-- FFI: pure md5 hash, used for historyId so that Allure can match test
-- results across runs (history/trends won't work without it)
foreign import md5Hash :: String -> String

-- FFI: sanitize a test name for use as a filename (String.replaceAll in PS
-- is NOT regex-based, so this has to happen in JS)
foreign import safeFilename_ :: String -> String

-- | Turn a test name into something safe for a filename
safeFilename :: String -> Int -> String
safeFilename s idx = safeFilename_ s <> "-" <> show idx

-- FFI: write a base64-encoded blob to a file, creating parent dirs as needed
foreign import writeBase64Sync_ :: String -> String -> Effect Unit

-- | A purescript-spec Reporter that writes one Allure result JSON file
-- | per test into the given output directory.
allureReporter :: String -> Reporter
allureReporter outputDir = go 0
  where
  go :: Int -> Reporter
  go idx = do
    event <- await
    case event of
      Event.TestEnd path name result -> do
        liftEffect $ writeResult idx path name result
        yield event
        go (idx + 1)

      Event.Pending path name -> do
        liftEffect $ writePending idx path name
        yield event
        go (idx + 1)

      other -> do
        yield other
        go idx

  suiteName :: Path -> String
  suiteName path = intercalate " > " (parentSuiteName path)

  fullTestName :: Path -> String -> String
  fullTestName path name = suiteName path <> " > " <> name

  -- Consume any attachments registered while the test ran: write their
  -- contents into the results directory and return the Allure attachment
  -- records for the result JSON.
  takePendingAttachments :: Int -> String -> Effect (Array Json)
  takePendingAttachments idx full = do
    atts <- Ref.read pendingAttachments
    _ <- Ref.write [] pendingAttachments
    sequence $ mapWithIndex (attachOne idx full) atts

  attachOne :: Int -> String -> Int -> Attachment -> Effect Json
  attachOne idx full i att = do
    let
      source =
        safeFilename (full <> "-" <> att.name) idx <> "-attachment-" <> show i
          <> att.fileExtension
    writeBase64Sync_ (outputDir <> "/" <> source) att.base64Contents
    pure $
         "name"   := att.name
      ~> "source" := source
      ~> "type"   := att.contentType
      ~> jsonEmptyObject

  writeResult :: Int -> Path -> String -> Result -> Effect Unit
  writeResult idx path name result = do
    now' <- now
    let
      full = fullTestName path name
      suite = suiteName path
      startMs = unwrap (unInstant now') :: Number

      { status, durationMs, message, trace } = case result of
        Success _ (Milliseconds ms) ->
          { status: "passed"
          , durationMs: Int.round ms
          , message: Nothing
          , trace: Nothing
          }
        Failure err ->
          { status: "failed"
          , durationMs: 0
          , message: Just (Error.message err)
          , trace: Error.stack err
          }

    attachmentJsons <-
      case result of
        Failure _ -> takePendingAttachments idx full
        _ -> pure []

    let
      json = mkAllureJson
        startMs
        durationMs
        attachmentJsons
        { name, fullName: full, suite, status, message, trace }

    writeFileSync_
      (outputDir <> "/" <> safeFilename full idx <> "-result.json")
      (stringify json)

  writePending :: Int -> Path -> String -> Effect Unit
  writePending idx path name = do
    now' <- now
    let
      full = fullTestName path name
      suite = suiteName path
      startMs = unwrap (unInstant now') :: Number
      json = mkAllureJson
        startMs
        0
        []
        { name
        , fullName: full
        , suite
        , status: "skipped"
        , message: Nothing
        , trace: Nothing
        }
    writeFileSync_
      (outputDir <> "/" <> safeFilename full idx <> "-result.json")
      (stringify json)


-- | Build the Allure result JSON object.
-- |
-- | Allure expects `start`/`stop` as epoch milliseconds (JSON numbers),
-- | and matches runs across reports (for history/trends) via `historyId`.
mkAllureJson
  :: Number
  -> Int
  -> Array Json
  -> { name :: String
     , fullName :: String
     , suite :: String
     , status :: String
     , message :: Maybe String
     , trace :: Maybe String
     }
  -> Json
mkAllureJson startMs durationMs attachments r =
  let
    labelJson :: String -> String -> Json
    labelJson n v =
      "name" := n ~> "value" := v ~> jsonEmptyObject

    labels =
      [ labelJson "framework" "purescript-spec"
      , labelJson "suite" r.suite
      , labelJson "language" "purescript"
      ]

    trace = case r.trace of
      Nothing -> ""
      Just t  -> t

    statusDetails = case r.message of
      Nothing -> jsonNull
      Just msg ->
        "message" := msg ~> "trace" := (trace :: String) ~> jsonEmptyObject

  in
         "name"          := r.name
      ~> "fullName"      := r.fullName
      ~> "status"        := r.status
      ~> "stage"         := "finished"
      ~> "start"         := startMs
      ~> "stop"          := (startMs + Int.toNumber durationMs)
      ~> "historyId"     := (md5Hash r.fullName)
      ~> "labels"        := labels
      ~> "statusDetails" := statusDetails
      ~> "links"         := ([] :: Array String)
      ~> "steps"         := ([] :: Array String)
      ~> "parameters"    := ([] :: Array String)
      ~> "attachments"   := attachments
      ~> jsonEmptyObject
