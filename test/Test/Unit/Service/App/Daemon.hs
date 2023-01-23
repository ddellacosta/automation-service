module Test.Unit.Service.App.Daemon
  ( spec_
  )
  where

import Control.Exception (evaluate)
import Test.Hspec (Spec, anyException, shouldThrow, shouldBe, it, describe)
import Test.QuickCheck (Testable(property))
-- import Test.Tasty (TestTree)
-- import Test.Tasty.Hspec (testSpec)

spec_ :: Spec
spec_ = describe "Prelude.head" $ do
  it "returns the first element of a list" $ do
    head [23 ..] `shouldBe` (23 :: Int)

  it "returns the first element of an *arbitrary* list" $
    property $ \x xs -> head (x:xs) == (x :: Int)

  it "throws an exception if used with an empty list" $ do
    evaluate (head []) `shouldThrow` anyException

  it "foos bar" $ do
    (1 :: Int) `shouldBe` (1 :: Int)
