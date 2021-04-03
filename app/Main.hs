module Main where

import Aws.Lambda
import qualified Scraper

main :: IO ()
main = runLambdaHaskellRuntime defaultDispatcherOptions Scraper.initializeAppConfig id (addStandaloneLambdaHandler "src/Scraper.handler" Scraper.handler)