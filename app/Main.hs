{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Aws.Lambda

import qualified Lib

-- This action is run on each cold start, and the context returned
-- is kept alive while the lambda is run, you can access it in your handler.
initializeContext :: IO ()
initializeContext = return ()

generateLambdaDispatcher StandaloneLambda defaultDispatcherOptions