module Test.Spec.Reporter.Allure (allureReporter, prepareResultsDir) where

import Data.Argonaut.Core (Json, jsonEmptyObject, jsonNull, stringify)
import Data.Argonaut.Encode ((:=), (~>))
import Data.DateTime.Instant (unInstant)
import Data.Foldable (intercalate)
import Data.Int (round, toNumber) as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception as Error
import Effect.Now (now)
import Pipes (await, yield)
import Prelude (($), (<>), (+), Unit, bind, discard, show)
import Test.Spec.Result (Result(..))
import Test.Spec.Runner (Reporter)
import Test.Spec.Runner.Event as Event
import Test.Spec.Tree (Path, parentSuiteName)

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

      json = mkAllureJson
        startMs
        durationMs
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
  -> { name :: String
     , fullName :: String
     , suite :: String
     , status :: String
     , message :: Maybe String
     , trace :: Maybe String
     }
  -> Json
mkAllureJson startMs durationMs r =
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
      ~> "attachments"   := ([] :: Array String)
      ~> jsonEmptyObject

-- | Turn a test name into something safe for a filename
safeFilename :: String -> Int -> String
safeFilename s idx = safeFilename_ s <> "-" <> show idx
