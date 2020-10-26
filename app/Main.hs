{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Aws.Lambda
import Aws.Lambda.Runtime.Common( LambdaError(..) )
import Control.Exception.Base
import qualified Lib
import Data.Text
import GHC.Exception.Type (SomeException)

-- This action is run on each cold start, and the context returned
-- is kept alive while the lambda is run, you can access it in your handler.
initializeContext :: IO ()
initializeContext = return ()


_run
 LambdaOptions {functionHandler = functionHandler,
                contextObject = contextObject, eventObject = eventObject,
                executionUuid = executionUuid} fh = do
    let returnErr_aIUv statusCode_aIUw
                   = (pure
                        . (Left
                             . (ApiGatewayLambdaError
                                  . mkApiGatewayResponse statusCode_aIUw)))
    case decodeObj eventObject of
       Right eventObject_aIUx
         -> do resultE_aIUy <- (try
                                  $ (fh eventObject_aIUx) contextObject)
               case resultE_aIUy of
                 Right result_aIUz
                   -> ((either
                          (pure
                             . (Left
                                  . (ApiGatewayLambdaError
                                       . fmap toApiGatewayResponseBody))))
                         (pure
                            . (Right
                                 . (ApiGatewayResult
                                      . fmap toApiGatewayResponseBody))))
                        result_aIUz
                 Left (handlerError_aIUA :: SomeException)
                   -> if ((propagateImpureExceptions . apiGatewayDispatcherOptions)
                            $ DispatcherOptions
                                (ApiGatewayDispatcherOptions True)) then
                          ((returnErr_aIUv 500 . (toApiGatewayResponseBody . show))
                             $ handlerError_aIUA)
                      else
                          ((returnErr_aIUv 500
                              . (toApiGatewayResponseBody . pack))
                             $ "Something went wrong.")
       Left err_aIUB
         -> ((returnErr_aIUv 400 . (toApiGatewayResponseBody . show))
               $ err_aIUB)


-- generateLambdaDispatcher UseWithAPIGateway defaultDispatcherOptions
main = (runLambda initializeContext) run
run LambdaOptions {functionHandler = functionHandler,
                contextObject = contextObject, eventObject = eventObject,
                executionUuid = executionUuid}
    = case functionHandler of
        "test" -> _run LambdaOptions {functionHandler = functionHandler,
                                                  contextObject = contextObject, eventObject = eventObject,
                                                  executionUuid = executionUuid} Lib.handler
        _ -> ((pure
                 . (Left
                      . (ApiGatewayLambdaError
                           . (mkApiGatewayResponse 500 . toApiGatewayResponseBody))))
                $ ("Handler "
                     <> (functionHandler <> " does not exist on project")))
