module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)
import qualified Test.Integration.Service.App.Daemon as Daemon


main :: IO ()
main = do
  daemonSpec <- testSpec "spec" Daemon.spec_
  defaultMain
    (testGroup "Unit Tests" [ daemonSpec ])

-- spec_ :: Spec
-- spec_ = describe "Prelude.head" $ do
--   it "returns the first element of a list" $ do
--     head [23 ..] `shouldBe` (23 :: Int)
-- 
--   it "returns the first element of an *arbitrary* list" $
--     property $ \x xs -> head (x:xs) == (x :: Int)
-- 
--   it "throws an exception if used with an empty list" $ do
--     evaluate (head []) `shouldThrow` anyException
-- 
--   it "foos bar" $ do
--     (1 :: Int) `shouldBe` (1 :: Int)
