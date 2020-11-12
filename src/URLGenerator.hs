module URLGenerator
    ( allocateURL
    , getURL) where

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
import Data.Text
import Debug.Trace
import Control.Monad.Loops (firstM)
import System.IO.Error
import Configuration


_s2av :: String -> AttributeValue
_s2av s = _t2av $ pack s

_t2av :: Text -> AttributeValue
_t2av t = attributeValue & avS .~ Just t

_filterUrls :: (MonadCatch m, MonadIO m, MonadUnliftIO m) => DynamoDBConfiguration -> String -> m Bool
_filterUrls conf url = do
    let item = fromList[((conf & table & keyField), _s2av url)]
    row <- fetchItem conf item
    let getItemRow = row ^. girsItem
    let result = Data.HashMap.Strict.null getItemRow
    return result


_findFreeURL :: DynamoDBConfiguration -> IO String
_findFreeURL conf = do
    keys <- generateKeys
    filteredResult <- firstM (_filterUrls conf) keys
    case filteredResult of
                 Nothing -> ioError (userError "No such Free Url")
                 Just x -> return x


allocateURL :: DynamoDBConfiguration -> String -> IO String
allocateURL conf url = do
    key <- _findFreeURL conf
    let itemKey = ((conf & table & keyField), (_s2av key))
    let itemValue = ((conf & table & valueField), (_s2av url))
    response <- insertItem conf (fromList[itemKey, itemValue])
    return key


getURL :: DynamoDBConfiguration -> String -> IO (Maybe String)
getURL conf key = do
    print $ "getURL for " ++ key
    print $ "Using configuration"
    print conf
    let item = fromList[((conf & table & keyField), (_s2av key))]
    row <- fetchItem conf item
    let rowItem = row ^. girsItem
    let field = conf & table & valueField
    let result = case fmap (\x -> (x ^. avS)) (Data.HashMap.Strict.lookup field rowItem) of
                      Nothing -> Nothing
                      Just x -> fmap unpack x
    return result
