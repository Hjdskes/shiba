module Scraper
  ( AppConfig(..)
  , initializeAppConfig
  , handler
  ) where

import Aws.Lambda
import Control.Lens            (set, (<&>))
import Control.Monad.Trans.AWS (LogLevel (..), envLogger, newEnv, newLogger)
import Data.IORef              (readIORef)
import Data.Text               (Text)
import Network.AWS             (Credentials (Discover), Env, Region (..),
                                Service)
import Network.AWS.DynamoDB    (dynamoDB)
import Scraper.Shiba
import System.IO               (stdout)

data AppConfig = AppConfig
  { env       :: Env
  , service   :: Service
  , region    :: Region
  , tableName :: Text
  }

initializeAppConfig :: IO AppConfig
initializeAppConfig = do
  logger <- newLogger Debug stdout
  env <- newEnv Discover <&> set envLogger logger
  return $ AppConfig env dynamoDB Ireland "scraper_key_value_store"

handler :: String -> Context AppConfig -> IO (Either String ())
handler _request context = do
  _appConfig <- readIORef $ customContext context
  scrape >>= \case
    Just scraped -> do
      putStrLn $ "Scraped " <> scraped
      return $ Right ()
    Nothing -> do
      putStrLn "Failed to scrape"
      return $ Left "Failed to scrape"
