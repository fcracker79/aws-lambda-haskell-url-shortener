module KeyGenerator where

import Utils
import Test.RandomStrings

urlPathChars = onlyAlphaNum randomASCII

numberOfAttempts :: Int -> Int
numberOfAttempts len = 10 ^ len

_generateKeysByLen :: Int -> [IO String]
_generateKeysByLen len = replicate (numberOfAttempts len) $ monadicFirst $ randomStringsLen (randomString urlPathChars) (len, len) 1

-- generateKeys :: [IO String]
-- generateKeys = foldM (++) (IO []) fmap _generateKeysByLen [1..]