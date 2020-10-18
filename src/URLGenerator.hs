module URLGenerator (allocateURL) where

import KeyGenerator
import DynamoDB
import Data.HashMap.Strict
import Network.AWS.DynamoDB
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.AWS
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Data.Text.Internal

_filterUrls :: (MonadCatch m, MonadIO m, MonadUnliftIO m) => DynamoDBConfiguration -> String -> m Bool
_filterUrls conf url = do
    let item = fromList[((conf & table & keyField), read url::AttributeValue)]
    row <- fetchItem conf item
    let getItemRow = row ^. girsItem
    let result = Data.HashMap.Strict.null getItemRow
    return result


_findFreeURL :: DynamoDBConfiguration -> IO String
_findFreeURL conf = do
    keys <- generateKeys
    filteredResult <- filterM (_filterUrls conf) keys
    return $ filteredResult!!0


allocateURL :: DynamoDBConfiguration -> String -> IO String
allocateURL conf url = do
    key <- _findFreeURL conf
    let itemKey = ((conf & table & keyField), read key::AttributeValue)
    let itemValue = ((conf & table & valueField), read url::AttributeValue)
    response <- insertItem conf (fromList[itemKey, itemValue])
    return key
