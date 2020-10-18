module KeyGeneratorSpec where


import Test.Hspec
import KeyGenerator

spec :: Spec
spec = do
  describe "Prelude.head" $ do
    it "The first keys must be 4 chars long" $ do
      keys <- generateKeys
      length (keys!!0) `shouldBe` 4
