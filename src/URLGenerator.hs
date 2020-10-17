module URLGenerator where

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
