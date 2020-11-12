module RecaptchaSecret where
import Network.AWS.SSM.GetParameter
import Network.AWS.SSM.Types
import Network.AWS
import Control.Monad.Trans.AWS
import System.Environment as Sysenv
import qualified Configuration as Conf
import System.IO
import Control.Lens
import Configuration
import qualified Data.Text as Text

getRecaptchaKey :: DynamoDBConfiguration -> IO String
getRecaptchaKey conf = do
    lgr <- newLogger Debug stdout
    env <- newEnv Discover <&> set envLogger lgr
    recaptchaSecretKey <- Text.pack <$> (Sysenv.getEnv "RECAPTCHA_SSM_PARAMETER_NAME")
    response <- runResourceT . runAWST env . Control.Monad.Trans.AWS.within (region conf) $ do
        Control.Monad.Trans.AWS.send $ getParameter recaptchaSecretKey

    let maybeParam = response ^. gprsParameter
    case maybeParam of
        Nothing -> ioError (userError "SSM Parameter not found")
        Just parameter -> do
            let parameterValue = parameter ^. pValue
            case parameterValue of
                Nothing -> ioError (userError "Missing SSM Parameter Value")
                Just v -> return $ Text.unpack v
