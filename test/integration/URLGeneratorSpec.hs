module URLGeneratorSpec where


import Test.Hspec
import URLGenerator
import DynamoDB
import Data.Text
import Network.AWS.Types
import Data.ByteString.Char8

spec :: Spec
spec = do
  describe "Prelude.head" $ do
    it "The saved URL must match with the fetched URL" $ do
      let fakeURL = "https://www.example.com"
      let conf = DynamoDBConfiguration {
          region=Ireland,
          endpoint=Just DynamoDBEndpoint {
              secure=False,
              host=Data.ByteString.Char8.pack "localhost",
              port=8000
          },
          table=DynamoDBTable {
              tablename="urls",
              keyField=Data.Text.pack "key",
              valueField=Data.Text.pack "url"
          }
      }
      key <- allocateURL conf fakeURL
      savedURL <- getURL conf key
      savedURL `shouldBe` Just fakeURL
