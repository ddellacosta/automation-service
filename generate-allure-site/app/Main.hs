{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Lens ((^?), to)
import Data.Aeson.Lens (_Array, _Integer, _String, key, nth)
import Data.ByteString.Lazy (ByteString, writeFile, readFile)
import Data.Foldable (fold, foldl', foldMap)
import Data.List (maximum, sortBy, zipWith)
import Data.Maybe (Maybe, fromMaybe)
import Data.String (String, fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (for)
import qualified Data.Vector as V
import Network.Wai.Application.Static (defaultFileServerSettings, staticApp)
import Network.Wai.Handler.Warp (run)
import Prelude (IO, Int, Integer, Show, (.), ($), (<$>), (<>), (-), (==), compare, flip, fst, max, mempty, pure, putStrLn, show, snd)
import System.Directory (listDirectory, setCurrentDirectory)
import System.Environment (getArgs, getProgName)
import Text.Blaze ((!))
import Text.Blaze.Html (Html, toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (customAttribute)
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import Text.Read (readMaybe)


allurePath :: String -> String -> String
allurePath branch suite = "allure-action/" <> branch <> "/" <> suite

integrity :: H.AttributeValue -> H.Attribute
integrity = customAttribute "integrity"

crossorigin :: H.AttributeValue -> H.Attribute
crossorigin = customAttribute "crossorigin"

dataBsToggle :: H.AttributeValue -> H.Attribute
dataBsToggle = customAttribute "data-bs-toggle"

dataBsTarget :: H.AttributeValue -> H.Attribute
dataBsTarget = customAttribute "data-bs-target"

dataBsParent :: H.AttributeValue -> H.Attribute
dataBsParent = customAttribute "data-bs-parent"

-- strings in Haskell, always fun
toText :: Show a => a -> Text
toText = T.pack . show

-- doing it in this convoluted way so that I can rely upon
-- JS providing l10n/i18n format for the client
localizedTimestamp :: String -> String -> Text -> Int -> Html
localizedTimestamp branch suite ts idx' = H.td $ do
  let
    tsId =
      T.pack branch <> "-" <> T.pack suite <> "-ts-" <> ts <> "-" <> (T.pack . show $ idx')
  H.div
    ! (A.id . fromString . T.unpack $ tsId)
    $ H.script $ toHtml $
        "document.getElementById('"
      <> tsId
      <> "').textContent = new Date("
      <> ts
      <> ").toLocaleString();"

--
-- Kinda bad how this will fail if one of these values is a Nothing,
-- but this is hacky utility code for generating test result views, so
-- if nothing shows up for a bunch of rows that's my signal this broke
-- and I need to check the data or make this more robust. For now it's
-- not worth the effort.
--
branchRunRowHtml :: String -> String -> Int -> ByteString -> Maybe (Integer, [Html])
branchRunRowHtml branch suite idx fileData = do
  runUniqueId <- fileData ^? nth idx . key "runUniqueId" . _String

  let
    newTimestampInteger = fromMaybe 0 $ fileData ^? nth idx . key "timestamp" . _Integer
    newTimestamp = toText newTimestampInteger

  passed <- toText <$> fileData ^? nth idx . key "summary" . key "statistic" . key "passed" . _Integer
  failed <- toText <$> fileData ^? nth idx . key "summary" . key "statistic" . key "failed" . _Integer
  broken <- toText <$> fileData ^? nth idx . key "summary" . key "statistic" . key "broken" . _Integer
  skipped <- toText <$> fileData ^? nth idx . key "summary" . key "statistic" . key "skipped" . _Integer
  total <- toText <$> fileData ^? nth idx . key "summary" . key "statistic" . key "total" . _Integer
  duration <- toText <$> fileData ^? nth idx . key "summary" . key "time" . key "duration" . _Integer
  testResult <- fileData ^? nth idx . key "testResult" . _String

  pure $
    ( newTimestampInteger
    , [H.tr
          ! A.class_ (passOrFailClass testResult)
          $ fold $
               [localizedTimestamp branch suite newTimestamp idx]
            <> (H.td . toHtml <$> [passed, failed, broken, skipped, total, duration])
            <> [H.td $ H.a ! A.href (testRunLink . T.unpack $ runUniqueId) $ toHtml runUniqueId]
      ]
    )

  where
    passOrFailClass = \case
      "PASS" -> "table-success"
      "FAIL" -> "table-danger"
      _ -> "table-secondary" -- ¯\_(ツ)_/¯

    testRunLink :: String -> H.AttributeValue
    testRunLink runUniqueId = fromString $
      "/" <> allurePath branch suite <> "/" <> runUniqueId <> "/"

--
-- Does the majority of the heavy lifting with stuffing branch rows
-- into Html (tr) and extracting the most recent timestamp from each
-- branch so we can compare later for sorting.
--
branchesRunsHtml :: [String] -> IO [(Integer, String, [(String, [Html])])]
branchesRunsHtml branches =
  for branches $ \branch -> do

    -- collects the most recent timestamp and tuples of
    -- suite/collected Html rows for that branch suite
    branchRuns :: [(Integer, (String, [Html]))] <-
      for ["frontend-tests", "backend-tests"] $ \suite -> do
        fileData <- readFile $ allurePath branch suite <> "/data.json"

        let
          -- lens is kind of insane
          length = fromMaybe 0 $ fileData ^? _Array . to V.length

          -- already exists somewhere?
          ffoldl' :: b -> [a] -> (b -> a -> b) -> b
          ffoldl' b ta f = foldl' f b ta

          -- returns the most recent timestamp for the set of runs in
          -- this branch/suite combo, along with the collected Html
          -- generated from each row
          (maxTimestamp', branchSuiteRuns) :: (Integer, [Html]) =
            ffoldl' (0, []) [0..(length - 1)] $ \(maxTimestamp, branchSuiteHtml) idx ->
              let
                (newTimestamp, branchSuiteRowHtml) =
                  fromMaybe (0, []) $
                    branchRunRowHtml branch suite idx fileData
              in
                (max newTimestamp maxTimestamp, branchSuiteHtml <> branchSuiteRowHtml)

        pure (maxTimestamp', (suite, branchSuiteRuns))

    -- return the most recent timestamp in the branch out of both
    -- suites, the branch name, and a list of each suite and its
    -- collected Html rows for all runs
    pure (maximum $ fst <$> branchRuns, branch, snd <$> branchRuns)

--
-- Takes the collected HTML for each given branch and displays it
-- inside Bootstrap accordions
-- (https://getbootstrap.com/docs/5.3/components/accordion/). The top
-- position (most recent branch built by CI) only will be uncollapsed
-- by default.
--
branchesHtml :: Int -> String -> [(String, [Html])] -> Html
branchesHtml pos branch branchRunsHtml = do
  let parentId = "branch-" <> branch
  H.h2 ! A.class_ "m-2 mt-4 p-1" $ toHtml branch
  H.div
    ! A.class_ "accordion"
    ! (A.id . fromString $ parentId) $
    H.div ! A.class_ "accordion-item" $
      (flip foldMap) branchRunsHtml $ \(suite, branchSuiteRuns) -> do
        let targetId = "collapse-" <> branch <> "-" <> suite
        H.h3 ! A.class_ "accordion-header" $
          H.button
          -- if we're at the top of the list (most recent) we show
          -- this opened up by default; otherwise we collapse
            ! (A.class_ $
                "accordion-button" <> (if pos == 0 then "" else " collapsed"))
            ! dataBsToggle "collapse"
            ! (dataBsTarget . fromString $ "#" <> targetId)
            ! A.type_ "button"
            $ toHtml suite
        H.div
        -- see above comment re: first position
          ! (A.class_ $
              "accordion-collapse collapse" <> (if pos == 0 then " show" else ""))
          ! (A.id . fromString $ targetId)
          ! (dataBsParent . fromString $ "#" <> parentId) $
          H.div ! A.class_ "accordion-body" $
            H.div ! A.class_ "accordion-item" $ branchLink branchSuiteRuns

  where
    branchLink :: [Html] -> Html
    branchLink testResults =
      H.table ! A.class_ "table" $ do
        H.thead $
          H.tr $
            foldMap (H.th . toHtml)
              (["Date", "Pass", "Fail", "Broken", "Skip", "Total", "Duration", "Test Run"] :: [Text])
        H.tbody $ fold testResults

--
-- Stuffs the collected branch HTML into the main page template
--
branchesPage :: Html -> Html
branchesPage branches = do
  H.html $ do
    H.head $ do
      H.title "automation-service - Branch Test Runs"
      H.link
        ! A.rel "stylesheet"
        ! A.type_ "text/css"
        ! A.href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.8/dist/css/bootstrap.min.css"
        ! integrity "sha384-sRIl4kxILFvY47J16cr9ZwB07vP4J8+LH7qKQnuqkuIAvNWLzeN8tE5YBujZqJLB"
        ! crossorigin "anonymous"

      H.script
        ! A.src "https://cdn.jsdelivr.net/npm/bootstrap@5.3.8/dist/js/bootstrap.bundle.min.js"
        ! integrity "sha384-FKyoEForCGlyvwx9Hj09JcYn3nv7wiPVlz7YYwJrWVcXK/BmnVDxM+D2scQbITxI"
        ! crossorigin "anonymous"
        $ mempty

    H.body $
      H.div ! A.class_ "container" $ do
        H.h2 ! A.class_ "m-2 p-1" $ "automation-service - Branch Test Runs"
        branches

--
-- this is hacky test page generation code and I'm not being very
-- careful about configuration, so keep in mind how many assumptions
-- this makes about where things are--this is deeply coupled to what
-- is set up in ./github/workflows/main.yml
--
generateSite :: IO ()
generateSite = do
  -- should probably make this configurable at some point
  setCurrentDirectory "ghp"
  files <- listDirectory "allure-action"
  branchesRuns :: [(Integer, String, [(String, [Html])])] <- branchesRunsHtml files
  let
    branchesRuns' = sortBy (\(ts1, _, _) (ts2, _, _) -> compare ts2 ts1) branchesRuns
    branchesRunsOutput =
      -- seems like there's a more obvious function to map over a list
      -- with an index that I'm missing but whatever
      zipWith
        (\idx (_ts, branch, branchRuns) -> branchesHtml idx branch branchRuns)
        [0..]
        branchesRuns'
  writeFile "test-runs/index.html" (renderMarkup $ branchesPage $ fold branchesRunsOutput)

serve :: IO ()
serve = do
  args <- getArgs
  let
    port = case args of
      (p:_) -> fromMaybe 8000 (readMaybe p)
      _     -> 8000
  putStrLn $ "Starting static file server on port " <> show port <> "..."
  run port $ staticApp (defaultFileServerSettings ".")

main :: IO ()
main = do
  progName <- getProgName
  case progName of
    "serve" -> serve
    _       -> generateSite
