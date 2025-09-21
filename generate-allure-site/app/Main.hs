{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString.Lazy (writeFile)
import Data.String (fromString)
import Network.Wai.Application.Static (defaultFileServerSettings, staticApp)
import Network.Wai.Handler.Warp (run)
import Prelude (FilePath, IO, (.), ($), (<>), foldMap, mapM_, mempty, putStrLn)
import System.Directory (getCurrentDirectory, listDirectory, setCurrentDirectory)
import System.Environment (getProgName)
import Text.Blaze ((!))
import Text.Blaze.Html (Html, toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (customAttribute)
import Text.Blaze.Renderer.Utf8 (renderMarkup)

integrity :: H.AttributeValue -> H.Attribute
integrity = customAttribute "integrity"

crossorigin :: H.AttributeValue -> H.Attribute
crossorigin = customAttribute "crossorigin"

branchLinks :: FilePath -> Html
branchLinks file =
  let
    branchLink n =
      H.a ! A.href (fromString $ "allure-action/" <> file <> "/" <> n <> "/index.html") $ toHtml (file <> "/" <> n <> "/index.html")
  in
    H.div $ do
      H.h4 . toHtml $ file
      H.ul $
        foldMap H.li [branchLink "frontend-tests", branchLink "backend-tests"]

branchesPage :: [FilePath] -> Html
branchesPage files = H.html $ do
  H.head $ do
    H.title "Branch Test Runs"
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

  H.body $ do
    H.h2 "Branch Test Runs"
    H.ul $
      foldMap (H.li . branchLinks) files

generateSite :: IO ()
generateSite = do
  setCurrentDirectory "ghp"
  cwd <- getCurrentDirectory
  files <- listDirectory "allure-action"
  putStrLn $ "cwd: " <> cwd
  mapM_ (\f -> putStrLn $ "  " <> f) files
  writeFile "index.html" (renderMarkup $ branchesPage files)

serve :: IO ()
serve = run 8000 $ staticApp (defaultFileServerSettings ".")

main :: IO ()
main = do
  progName <- getProgName
  case progName of
    "serve" -> serve
    _       -> generateSite
