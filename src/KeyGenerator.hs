module KeyGenerator where

import Utils
import Test.RandomStrings

_minLength = 4
_urlPathChars = onlyAlphaNum randomASCII

_numberOfAttempts :: Int -> Int
_numberOfAttempts len = 10 ^ len

_generateKeysByLen :: Int -> IO [String]
_generateKeysByLen len = randomStringsLen (randomString _urlPathChars) (len, len) (_numberOfAttempts len)

_recGenerateKeys :: Int -> IO [String]
_recGenerateKeys i = pure (++) <*> (_generateKeysByLen i) <*> (_generateKeysByLen (i + 1))
generateKeys = _recGenerateKeys _minLength