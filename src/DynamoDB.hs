module DynamoDB where


import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS
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

insertItem region secure host port table item = do
    lgr <- newLogger Debug stdout
    env <- newEnv Discover <&> set envLogger lgr

    -- Specify a custom DynamoDB endpoint to communicate with:
    let dynamo = setEndpoint secure host port dynamoDB

    runResourceT . runAWST env . within region $ do
        -- Scoping the endpoint change using 'reconfigure':
        reconfigure dynamo $ do
            say $ "Inserting item into table '"
               <> table
               <> "' with attribute names: "
               <> Text.intercalate ", " (Map.keys item)
            -- Insert the new item into the specified table:
            send $ putItem table & piItem .~ item


say :: MonadIO m => Text -> m ()
say = liftIO . Text.putStrLn
