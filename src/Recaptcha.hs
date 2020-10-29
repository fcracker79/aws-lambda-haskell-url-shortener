module Recaptcha where


{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Req
import Data.Text as DT
import Network.HTTP.Req (ReqBodyUrlEnc (..), ReqBodyMultipart, reqBodyMultipart)
import Network.HTTP.Client.MultipartFormData ( partBS )
import Data.ByteString.Char8 as BS


type RecaptchaSecretKey = String
type RecaptchaToken = String



verifyCaptcha :: Network.HTTP.Req.MonadHttp m => RecaptchaSecretKey -> RecaptchaToken -> m Bool
verifyCaptcha secretKey token = do
    response <-
        req
          POST
          (https "www.google.com" /: "recaptcha/api/siteverify")
          (ReqBodyUrlEnc $ "secret" =: secretKey <> "response" =: token)
          jsonResponse
          mempty
    let jsonResponse = (responseBody response :: Value)
    -- TODO
    return True