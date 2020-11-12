module Configuration (
    getConf
    , DynamoDBEndpoint (..)
    , DynamoDBTable (..)
    , DynamoDBConfiguration (..)
    , DynamoDBItem (..)
    ) where

import Data.Text
import qualified Data.Text as Text
import System.Environment as Sysenv
import Network.AWS
import Network.AWS.Data
import           Data.ByteString         (ByteString)
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as Map
import           Network.AWS.DynamoDB

data DynamoDBEndpoint = DynamoDBEndpoint {
    secure :: Bool,
    host :: ByteString,
    port :: Int
} deriving(Show)

data DynamoDBTable = DynamoDBTable {
    tablename :: String,
    keyField :: Text,
    valueField :: Text
} deriving(Show)

data DynamoDBConfiguration = DynamoDBConfiguration {
    region :: Region,
    endpoint :: Maybe DynamoDBEndpoint,
    table :: DynamoDBTable
} deriving(Show)

type DynamoDBItem = HashMap Text AttributeValue


getConf :: IO DynamoDBConfiguration
getConf = do
    regionString <- Sysenv.getEnv "AWS_REGION"
    tableName <- Sysenv.getEnv "URLS_TABLE_NAME"
    print $ "Region " ++ regionString
    case (fromText (Text.pack regionString)::(Either String Region)) of
        Right region -> return (DynamoDBConfiguration {
                region=region,
                endpoint=Nothing,
                table=DynamoDBTable {
                    tablename=tableName,
                    keyField=Data.Text.pack "id",
                    valueField=Data.Text.pack "url"
                }
                })
        Left _ -> ioError (userError $ "Invalid region" ++ regionString)
