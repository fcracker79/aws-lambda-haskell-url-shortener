module KeyGenerator where

import Test.RandomStrings
import System.IO.Unsafe

urlPathChars = onlyAlphaNum randomASCII

numberOfAttempts :: Int -> Int
numberOfAttempts len = 10 ^ len

_generateKeysByLen :: Int -> [String]
_generateKeysByLen len = unsafePerformIO $ randomStringsLen (randomString urlPathChars) (len, len) (numberOfAttempts len)

-- generateKeys :: IO [String]
-- generateKeys = foldM (++) (IO []) fmap _generateKeysByLen [1..]