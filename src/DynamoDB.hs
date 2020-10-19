module DynamoDB
    ( insertItem
    , fetchItem
    , DynamoDBEndpoint (..)
    , DynamoDBTable (..)
    , DynamoDBConfiguration (..)
    , DynamoDBItem (..)) where


import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS
import           Control.Monad.Catch
import           Control.Monad.IO.Unlift
import           Data.ByteString         (ByteString)
import           Data.Conduit
import qualified Data.Conduit.List       as CL
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as Map
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import           Network.AWS.Data
import           Network.AWS.DynamoDB
import           System.IO
import           Debug.Trace

data DynamoDBEndpoint = DynamoDBEndpoint {
    secure :: Bool,
    host :: ByteString,
    port :: Int
}

data DynamoDBTable = DynamoDBTable {
    tablename :: String,
    keyField :: Text,
    valueField :: Text
}

data DynamoDBConfiguration = DynamoDBConfiguration {
    region :: Region,
    endpoint :: Maybe DynamoDBEndpoint,
    table :: DynamoDBTable
}

type DynamoDBItem = HashMap Text AttributeValue


_createService :: Maybe DynamoDBEndpoint -> Service
_createService Nothing = dynamoDB
_createService (Just endpoint) = setEndpoint (secure endpoint) (host endpoint) (port endpoint) dynamoDB

insertItem
  :: (MonadCatch m, MonadIO m, MonadUnliftIO m) =>
      DynamoDBConfiguration -> DynamoDBItem -> m PutItemResponse
insertItem conf item = do
    lgr <- newLogger Debug stdout
    env <- newEnv Discover <&> set envLogger lgr

    -- Specify a custom DynamoDB endpoint to communicate with:
    let dynamo = _createService (endpoint conf)
    let tableName = Text.pack (conf & table & tablename)
    say $ Text.pack "********** insertItem **********"
    say $ tableName
    say $ Text.pack $ show (region conf)
    runResourceT . runAWST env . within (region conf) $ do
        -- Scoping the endpoint change using 'reconfigure':
        reconfigure dynamo $ do
            say $ "Inserting item into table '"
               -- conf.table.tablename is a Text. Thanks god, Text is a Read instance too
               <> tableName
               <> "' with attribute names: "
               <> Text.intercalate ", " (Map.keys item)
            -- Insert the new item into the specified table:
            send $ putItem tableName & piItem .~ item


fetchItem
  :: (MonadCatch m, MonadIO m, MonadUnliftIO m) =>
      DynamoDBConfiguration -> DynamoDBItem -> m GetItemResponse
fetchItem conf key = do
    lgr <- newLogger Debug stdout
    env <- newEnv Discover <&> set envLogger lgr

    -- Specify a custom DynamoDB endpoint to communicate with:
    let dynamo = _createService (endpoint conf)
    let tableName = Text.pack (conf & table & tablename)
    say $ Text.pack "+++++++++++ fetchItem +++++++++++++"
    say $ tableName
    say $ Text.pack $ show (region conf)
    runResourceT . runAWST env . within (region conf) $ do
        -- Scoping the endpoint change using 'reconfigure':
        reconfigure dynamo $ do
            send $ getItem tableName & giKey .~ key

say :: MonadIO m => Text -> m ()
say = liftIO . Text.putStrLn
