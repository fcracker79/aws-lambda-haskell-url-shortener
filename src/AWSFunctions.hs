module AWSFunctions where


import GHC.Generics
import Aws.Lambda
import Network.HTTP.Types.Header
import Data.ByteString.Char8
import System.Environment as Sysenv
import Data.Text
import URLGenerator
import DynamoDB
import Network.AWS
import Network.AWS.Data

_getConf :: IO DynamoDBConfiguration
_getConf = do
    region <- Sysenv.getEnv "AWS_REGION"
    return (DynamoDBConfiguration {
        region=read region::Region,
        endpoint=Nothing,
        table=DynamoDBTable {
            tablename="Urls",
            keyField=Data.Text.pack "id",
            valueField=Data.Text.pack "url"
        }
        })


redirectUrl :: (ApiGatewayRequest String) -> Context () -> IO (Either (ApiGatewayResponse String) (ApiGatewayResponse String))
redirectUrl request context = do
    conf <- _getConf
    let shortenerId = Prelude.head $ Prelude.filter
            (\x -> Data.Text.length x > 0)
            $ Data.Text.splitOn (Data.Text.pack "/") $ apiGatewayRequestPath request
    maybeUrl <- URLGenerator.getURL conf $ Data.Text.unpack shortenerId
    case maybeUrl of
        Just url -> return $ Right $ ApiGatewayResponse {
                                  apiGatewayResponseStatusCode = 301,
                                  apiGatewayResponseIsBase64Encoded = False,
                                  apiGatewayResponseBody = "",
                                  apiGatewayResponseHeaders = [
                                      (hLocation, Data.ByteString.Char8.pack url)
                                  ]
                          }
        Nothing -> return $ Right $ ApiGatewayResponse {
                                 apiGatewayResponseStatusCode = 404,
                                 apiGatewayResponseIsBase64Encoded = False,
                                 apiGatewayResponseBody = "",
                                 apiGatewayResponseHeaders = [

                                 ]
                         }
