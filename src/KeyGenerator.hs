module KeyGenerator where

import Utils
import Test.RandomStrings

_minLength = 4
_urlPathChars = onlyAlphaNum randomASCII

_numberOfAttempts :: Int -> Int
_numberOfAttempts len = 10 ^ len

_generateKeysByLen :: Int -> [IO String]
_generateKeysByLen len = replicate (_numberOfAttempts len) $ monadicFirst $ randomStringsLen (randomString _urlPathChars) (len, len) 1

_recGenerateKeys :: Int -> [IO String]
_recGenerateKeys i = (_generateKeysByLen i) ++ (_recGenerateKeys (i + 1))

generateKeys :: [IO String]
generateKeys = _recGenerateKeys _minLength
