module Main where

import           Aws.Lambda
import qualified Config
import qualified Scraper

main :: IO ()
main = runLambdaHaskellRuntime defaultDispatcherOptions Config.initializeAppConfig id (addStandaloneLambdaHandler "src/Scraper.handler" Scraper.handler)
