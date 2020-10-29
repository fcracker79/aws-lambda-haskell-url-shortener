module Recaptcha where


{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Aeson            (Value)
import Network.Wreq
import Control.Lens
import OpenSSL.Session (context)
import Network.HTTP.Client.OpenSSL
import Data.Text as DT

type RecaptchaSecretKey = String
type RecaptchaToken = String


verifyCaptcha :: RecaptchaSecretKey -> RecaptchaToken -> IO Bool
verifyCaptcha secretKey token = do
    let opts = defaults & manager .~ Left (opensslManagerSettings context)
    r2 <- post "http://httpbin.org/post" ["num" := 3, "str" := "wat"]
    r <- withOpenSSL $
         postWith
             opts
             "https://www.google.com/recaptcha/api/siteverify"
             ["secret" := secretKey, "response" := token]
             --[partText (DT.pack "secret") (DT.pack secretKey), partText (DT.pack "response") (DT.pack token)]
    -- TODO
    return True