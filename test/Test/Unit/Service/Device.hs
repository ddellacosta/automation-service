module Test.Unit.Service.Device
  ( spec
  ,
  )
where

import Test.Hspec (Spec, describe, it, shouldBe)
-- import Service.Device (Device(..), DeviceId(..), findDevice, nullDevice)

spec :: Spec
spec = describe "Device logic" $ do
   it "finds devices by DeviceId from a list of Devices" $ do
     (1 :: Int) `shouldBe` (2 :: Int)
--     let gledoptoglc007p =
--           Device GledoptoGLC007P_1 "Gledopto GLC-007-P" "gledopto glc-007-p"
--     findDevice GledoptoGLC007P_1 [ gledoptoglc007p ] `shouldBe` gledoptoglc007p
-- 
--   it "returns nullDevice when DeviceId isn't found in the list of Devices" $ do
--     findDevice GledoptoGLC007P_1 [] `shouldBe` nullDevice
