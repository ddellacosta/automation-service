module Test.Unit.Service.Device
  ( spec_
  ,
  )
where

import Test.Hspec (Spec, describe, it, shouldBe)
import Service.Device (Device(..), DeviceId(..), findDevice, nullDevice)

spec_ :: Spec
spec_ = describe "Device logic" $ do
  it "finds devices by DeviceId from a list of Devices" $ do
    let gledoptoglc007p =
          Device GledoptoGLC007P_1 "Gledopto GLC-007-P" "gledopto glc-007-p"
    findDevice GledoptoGLC007P_1 [ gledoptoglc007p ] `shouldBe` gledoptoglc007p

  it "returns nullDevice when DeviceId isn't found in the list of Devices" $ do
    findDevice GledoptoGLC007P_1 [] `shouldBe` nullDevice
