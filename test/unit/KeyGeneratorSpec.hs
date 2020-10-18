module KeyGeneratorSpec where


import Test.Hspec
import KeyGenerator

spec :: Spec
spec = do
  describe "Prelude.head" $ do
    it "The first keys must be 4 chars long" $ do
      keys <- generateKeys
      let first1000Keys = take 1000 keys
      mconcat $ fmap (\x -> length x `shouldBe` 4) $ first1000Keys
    it "The 100000th element must be 5 chars long" $ do
      keys <- generateKeys
      length (keys!!9999) `shouldBe` 4
      length (keys!!10000) `shouldBe` 5
