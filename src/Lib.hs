module Lib where

import GHC.Generics
import Data.Aeson
import DynamoDB
import Aws.Lambda

data Person = Person
  { personName :: String
  , personAge :: Int
  } deriving (Generic)

instance FromJSON Person
instance ToJSON Person

-- handler :: Person -> Context () -> IO (Either String Person)
-- handler person context =
--   if personAge person > 0 then
--     return (Right person)
--   else
--     return (Left "A person's age must be positive")


handler :: (ApiGatewayRequest String) -> Context () -> IO (Either (ApiGatewayResponse String) (ApiGatewayResponse String))
handler request context = do
    let url = resource request
    let payload = reqBody request
    return $ Right $ ApiGatewayResponse {
        apiGatewayResponseStatusCode = 200,
        apiGatewayResponseBody = "TODO",
        apiGatewayResponseIsBase64Encoded = False,
        apiGatewayResponseHeaders =
    }
