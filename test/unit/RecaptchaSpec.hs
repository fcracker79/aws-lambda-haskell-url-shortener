module RecaptchaSpec where

{-# LANGUAGE OverloadedStrings #-}

import Recaptcha
import Test.Hspec
import KeyGenerator

spec :: Spec
spec = do
  describe "Prelude.head" $ do
    it "Recaptcha check must fail with invalid key" $ do
        let key = "invalidKey"::String
        let token = "arbitraryToken"::String
        result <- verifyCaptcha key token
        result `shouldBe` False
