module AWSFunctions (redirectUrl, createUrl) where

{-# LANGUAGE OverloadedStrings #-}

import GHC.Generics
import Aws.Lambda
import Network.HTTP.Types.Header
import Data.ByteString.Char8
import Data.ByteString.Lazy.Char8
import System.Environment as Sysenv
import Data.Text
import URLGenerator
import DynamoDB
import Network.AWS
import Network.AWS.Data
import Web.FormUrlEncoded
import Recaptcha
import qualified Data.Text as Text


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


-- TODO migrate to SSM or secrets-manager (beware, currently not available in all regions)
_getRecaptchaKey :: IO String
_getRecaptchaKey = Sysenv.getEnv "RECAPTCHA_KEY"


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


createUrl :: (ApiGatewayRequest String) -> Context () -> IO (Either (ApiGatewayResponse String) (ApiGatewayResponse String))
createUrl request context = do
    conf <- _getConf
    let maybeBody = apiGatewayRequestBody request
    case maybeBody of
        Just body -> _createAndRedirectToMain body
        Nothing -> return $ Right $ ApiGatewayResponse {
                                    apiGatewayResponseStatusCode = 400,
                                    apiGatewayResponseIsBase64Encoded = False,
                                    apiGatewayResponseBody = "",
                                    apiGatewayResponseHeaders = [

                                    ]
                            }


data SubmitUrlForm = SubmitUrlForm {
                      url :: String,
                      recaptchaToken :: String
}

instance FromForm SubmitUrlForm where
    fromForm f = SubmitUrlForm
        <$> parseUnique "url" f
        <*> parseUnique "g-recaptcha-response"  f

_createAndRedirectToMain :: String -> IO (Either (ApiGatewayResponse String) (ApiGatewayResponse String))
_createAndRedirectToMain body = do
    let form = urlDecodeAsForm $ Data.ByteString.Lazy.Char8.pack body
    case form of
        Left x -> return $ Right $ ApiGatewayResponse {
                                    apiGatewayResponseStatusCode = 400,
                                    apiGatewayResponseIsBase64Encoded = False,
                                    apiGatewayResponseBody = Text.unpack x,
                                    apiGatewayResponseHeaders = [

                                    ]
        }
        Right (SubmitUrlForm urlToStore recaptchaTokenValue) -> do
            recaptchaKey <- _getRecaptchaKey
            tokenVerified <- verifyCaptcha recaptchaKey $ recaptchaTokenValue
            if tokenVerified then _storeAndRedirect urlToStore
                             else return $ Right $ ApiGatewayResponse {
                                                       apiGatewayResponseStatusCode = 400,
                                                       apiGatewayResponseIsBase64Encoded = False,
                                                       apiGatewayResponseBody = "",
                                                       apiGatewayResponseHeaders = [

                                                       ]
                                                   }

_storeAndRedirect :: String -> IO (Either (ApiGatewayResponse String) (ApiGatewayResponse String))
_storeAndRedirect url = do
    conf <- _getConf
    redirectUrlWithUriParam <- (++) <$> (pure "/?uri=") <*> (allocateURL conf url)
    return $ Right $ ApiGatewayResponse {
            apiGatewayResponseStatusCode = 301,
            apiGatewayResponseIsBase64Encoded = False,
            apiGatewayResponseBody = "",
            apiGatewayResponseHeaders = [
                (hLocation, Data.ByteString.Char8.pack redirectUrlWithUriParam)
            ]
    }