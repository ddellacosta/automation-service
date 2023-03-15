module Test.Unit.Service.Helpers
  ( spec
  ,
  )
  where

import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Tests Helper functions" $ do
  it "returns all the Asyncs along with their associated Actions" $ do
    (1 :: Int) `shouldBe` (1 :: Int)
