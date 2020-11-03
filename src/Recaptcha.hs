module Recaptcha where


{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Req
import Data.Text as DT
import Network.HTTP.Req (ReqBodyUrlEnc (..), ReqBodyMultipart, reqBodyMultipart)
import Network.HTTP.Client.MultipartFormData ( partBS )
import Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM

type RecaptchaSecretKey = String
type RecaptchaToken = String


-- verifyCaptcha :: MonadIO m => RecaptchaSecretKey -> RecaptchaToken -> m Bool
verifyCaptcha secretKey token = do
    response <-
        runReq defaultHttpConfig $ do
            r <- req
                     POST
                     (https "www.google.com" /: "recaptcha" /: "api" /: "siteverify")
                     (ReqBodyUrlEnc $ "secret" =: secretKey <> "response" =: token)
                     jsonResponse
                     mempty
            let Object body = (responseBody r :: Value)
            (liftIO . print) body
            case (HM.lookup "success" body) of
                Nothing -> return False
                Just (Bool x) -> return x
                Just _ -> return False
    return response