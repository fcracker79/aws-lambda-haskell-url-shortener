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
import Control.Monad.IO.Class
import qualified Data.Text as Text
import System.IO.Error
import qualified Configuration as Conf
import RecaptchaSecret


redirectUrl :: (ApiGatewayRequest String) -> Context () -> IO (Either (ApiGatewayResponse String) (ApiGatewayResponse String))
redirectUrl request context = do
    (liftIO . print) "Executing URL redirection"
    (liftIO . print) request
    conf <- Conf.getConf
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
    (liftIO . print) "Executing URL creation"
    (liftIO . print) request
    conf <- Conf.getConf
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
            conf <- Conf.getConf
            recaptchaKey <- getRecaptchaKey conf
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
    conf <- Conf.getConf
    redirectUrlWithUriParam <- (++) <$> (pure "/?uri=") <*> (allocateURL conf url)
    return $ Right $ ApiGatewayResponse {
            apiGatewayResponseStatusCode = 301,
            apiGatewayResponseIsBase64Encoded = False,
            apiGatewayResponseBody = "",
            apiGatewayResponseHeaders = [
                (hLocation, Data.ByteString.Char8.pack redirectUrlWithUriParam)
            ]
    }